package gov.idaho.isp.saktrack.domain.user.organization;

import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent.EventType;
import gov.idaho.isp.saktrack.domain.EventFlag;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.dto.CreateKitEventDetails;
import gov.idaho.isp.saktrack.domain.dto.EventDetails;
import gov.idaho.isp.saktrack.event.KitCreateEvent;
import gov.idaho.isp.saktrack.event.KitDestroyEvent;
import gov.idaho.isp.saktrack.event.KitReceiveEvent;
import gov.idaho.isp.saktrack.event.KitReleasedForReviewEvent;
import gov.idaho.isp.saktrack.event.KitRepurposeEvent;
import gov.idaho.isp.saktrack.event.KitReviewEvent;
import gov.idaho.isp.saktrack.event.KitSendEvent;
import gov.idaho.isp.saktrack.exception.SexualAssaultKitTrackingException;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;

@Service
class UserKitServiceImpl implements UserKitService {
  private final OrganizationRepository organizationRepository;
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final ApplicationEventPublisher applicationEventPublisher;
  private final TransitionValidationService transitionValidationService;

  public UserKitServiceImpl(OrganizationRepository organizationRepository, SexualAssaultKitRepository sexualAssaultKitRepository, ApplicationEventPublisher applicationEventPublisher, TransitionValidationService transitionValidationService) {
    this.organizationRepository = organizationRepository;
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.applicationEventPublisher = applicationEventPublisher;
    this.transitionValidationService = transitionValidationService;
  }

  @Override
  public void create(LabUser user, CreateKitEventDetails eventDetails) throws SexualAssaultKitTrackingException {
    List<SexualAssaultKit> kits = new ArrayList<>();
    Set<String> errors = new HashSet<>();
    KitValidationStrategy validationStrategy = transitionValidationService.getCreateValidationStrategy(user, eventDetails);

    eventDetails.getSerialNumberList().forEach(serialNumber -> {
      try {
        SexualAssaultKit kit = createNewKit(serialNumber, user);
        validationStrategy.validate(kit, serialNumber);

        ChainOfCustodyEvent event = new ChainOfCustodyEventBuilder()
          .eventType(EventType.CREATE)
          .eventDate(eventDetails.getEventDate())
          .notes(eventDetails.getNotes())
          .actor(user)
          .to(user.getOrganization())
          .build();

        kit.setExpirationDate(eventDetails.getExpirationDate());
        kit.addChainOfCustodyEvent(event);
        kits.add(kit);
      }
      catch (SexualAssaultKitTrackingException ex) {
        errors.addAll(ex.getErrors());
      }
    });

    if (!errors.isEmpty()) {
      throw new SexualAssaultKitTrackingException("Error processing kits.", errors);
    }
    sexualAssaultKitRepository.saveAll(kits);
    applicationEventPublisher.publishEvent(new KitCreateEvent(user, eventDetails));
  }

  private SexualAssaultKit createNewKit(String serialNumber, LabUser actor) {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber(serialNumber);
    kit.setCurrentAssignment(actor.getOrganization());
    return kit;
  }

  @Override
  public void delete(LabUser user, EventDetails eventDetails) throws SexualAssaultKitTrackingException {
    List<SexualAssaultKit> kitsToDelete = new ArrayList<>();
    Set<String> errors = new HashSet<>();
    KitValidationStrategy validationStrategy = transitionValidationService.getDeleteValidationStrategy(user, eventDetails);

    eventDetails.getSerialNumberList().forEach(serialNumber -> {
      try {
        SexualAssaultKit kit = sexualAssaultKitRepository.findBySerialNumber(serialNumber);
        validationStrategy.validate(kit, serialNumber);
        kitsToDelete.add(kit);
      }
      catch (SexualAssaultKitTrackingException ex) {
        errors.addAll(ex.getErrors());
      }
    });

    if (!errors.isEmpty()) {
      throw new SexualAssaultKitTrackingException("Error processing kits.", errors);
    }

    sexualAssaultKitRepository.deleteAll(kitsToDelete);
  }

  @Override
  public void send(OrganizationUser user, EventDetails eventDetails, OrganizationType destinationType) throws SexualAssaultKitTrackingException {
    Organization sendTo = organizationRepository.findById(eventDetails.getOrgId()).orElse(null);
    List<SexualAssaultKit> kits = new ArrayList<>();
    Set<String> errors = new HashSet<>();
    KitValidationStrategy validationStrategy = transitionValidationService.getSendValidationStrategy(user, eventDetails, sendTo, destinationType);

    eventDetails.getSerialNumberList().forEach(serialNumber -> {
      try {
        SexualAssaultKit kit = sexualAssaultKitRepository.findBySerialNumber(serialNumber);
        validationStrategy.validate(kit, serialNumber);

        ChainOfCustodyEvent event = new ChainOfCustodyEventBuilder()
          .eventType(EventType.SEND)
          .eventDate(eventDetails.getEventDate())
          .notes(eventDetails.getNotes())
          .actor(user)
          .from(user.getOrganization())
          .to(sendTo)
          .eventFlag(EventFlag.flagSend(user, kit, sendTo).orElse(null))
          .build();

        kit.addChainOfCustodyEvent(event);
        kit.setCurrentAssignment(sendTo);
        kits.add(kit);
      }
      catch (SexualAssaultKitTrackingException ex) {
        errors.addAll(ex.getErrors());
      }
    });

    if (!errors.isEmpty()) {
      throw new SexualAssaultKitTrackingException("Error processing kits.", errors);
    }
    sexualAssaultKitRepository.saveAll(kits);
    applicationEventPublisher.publishEvent(new KitSendEvent(user, eventDetails));
  }

  @Override
  public void receive(OrganizationUser user, EventDetails eventDetails) throws SexualAssaultKitTrackingException {
    Organization sendFrom = organizationRepository.findById(eventDetails.getOrgId()).orElse(null);

    List<SexualAssaultKit> kits = new ArrayList<>();
    Set<String> errors = new HashSet<>();
    KitValidationStrategy validationStrategy = transitionValidationService.getReceiveValidationStrategy(user, eventDetails, sendFrom);

    eventDetails.getSerialNumberList().forEach(serialNumber -> {
      try {
        SexualAssaultKit kit = sexualAssaultKitRepository.findBySerialNumber(serialNumber);
        validationStrategy.validate(kit, serialNumber);

        ChainOfCustodyEvent event = new ChainOfCustodyEventBuilder()
          .eventType(EventType.RECEIVE)
          .eventDate(eventDetails.getEventDate())
          .notes(eventDetails.getNotes())
          .actor(user)
          .from(sendFrom)
          .to(user.getOrganization())
          .eventFlag(EventFlag.flagReceive(user, kit, sendFrom).orElse(null))
          .build();

        if (!Objects.equals(user.getOrganization(), kit.getCurrentAssignment())) {
          kit.setQuestionableEvents(true);
          event.setGrabbedOutOfOrder(true);
        }

        kit.addChainOfCustodyEvent(event);
        kit.setCurrentAssignment(user.getOrganization());
        kits.add(kit);
      }
      catch (SexualAssaultKitTrackingException ex) {
        errors.addAll(ex.getErrors());
      }
    });

    if (!errors.isEmpty()) {
      throw new SexualAssaultKitTrackingException("Error processing kits.", errors);
    }
    sexualAssaultKitRepository.saveAll(kits);
    applicationEventPublisher.publishEvent(new KitReceiveEvent(user, eventDetails));
  }

  @Override
  public void destroy(LawEnforcementUser user, EventDetails eventDetails) throws SexualAssaultKitTrackingException {
    List<SexualAssaultKit> kits = new ArrayList<>();
    Set<String> errors = new HashSet<>();
    KitValidationStrategy validationStrategy = transitionValidationService.getDestroyValidationStrategy(user, eventDetails);

    eventDetails.getSerialNumberList().forEach(serialNumber -> {
      try {
        SexualAssaultKit kit = sexualAssaultKitRepository.findBySerialNumber(serialNumber);
        validationStrategy.validate(kit, serialNumber);

        ChainOfCustodyEvent event = new ChainOfCustodyEventBuilder()
          .eventType(EventType.DESTROY)
          .eventDate(eventDetails.getEventDate())
          .notes(eventDetails.getNotes())
          .actor(user)
          .from(user.getOrganization())
          .build();

        kit.addChainOfCustodyEvent(event);
        kit.setCurrentAssignment(null);
        kits.add(kit);
      }
      catch (SexualAssaultKitTrackingException ex) {
        errors.addAll(ex.getErrors());
      }
    });

    if(!errors.isEmpty()) {
      throw new SexualAssaultKitTrackingException("Error processing kits.", errors);
    }
    sexualAssaultKitRepository.saveAll(kits);
    applicationEventPublisher.publishEvent(new KitDestroyEvent(user, eventDetails));
  }

  @Override
  public void repurpose(OrganizationUser user, EventDetails eventDetails) throws SexualAssaultKitTrackingException {
    List<SexualAssaultKit> kits = new ArrayList<>();
    Set<String> errors = new HashSet<>();
    KitValidationStrategy validationStrategy = transitionValidationService.getRepurposeValidationStrategy(user, eventDetails);

    eventDetails.getSerialNumberList().forEach(serialNumber -> {
      try {
        SexualAssaultKit kit = sexualAssaultKitRepository.findBySerialNumber(serialNumber);
        validationStrategy.validate(kit, serialNumber);

        ChainOfCustodyEvent event = new ChainOfCustodyEventBuilder()
          .eventType(EventType.REPURPOSE)
          .eventDate(eventDetails.getEventDate())
          .notes(eventDetails.getNotes())
          .actor(user)
          .from(user.getOrganization())
          .build();

        kit.addChainOfCustodyEvent(event);
        kit.setCurrentAssignment(null);
        kits.add(kit);
      }
      catch (SexualAssaultKitTrackingException ex) {
        errors.addAll(ex.getErrors());
      }
    });

    if (!errors.isEmpty()) {
      throw new SexualAssaultKitTrackingException("Error processing kits.", errors);
    }
    sexualAssaultKitRepository.saveAll(kits);
    applicationEventPublisher.publishEvent(new KitRepurposeEvent(user, eventDetails));
  }

  @Override
  public void releaseForProsecutorReview(LawEnforcementUser user, SexualAssaultKit kit) throws SexualAssaultKitTrackingException {
    KitValidationStrategy validationStrategy = transitionValidationService.getReleaseForReviewValidationStrategy(user);
    validationStrategy.validate(kit, kit.getSerialNumber());

    kit.getLegalDetails().setReleasedForReview(LocalDate.now());
    kit.getLegalDetails().setNonSubmissionReason(kit.getLeDetails().getNonSubmissionReason());

    sexualAssaultKitRepository.save(kit);
    applicationEventPublisher.publishEvent(new KitReleasedForReviewEvent(user, kit));
  }

  @Override
  public void review(LegalUser user, SexualAssaultKit kit, String notes, boolean agree) throws SexualAssaultKitTrackingException {
    KitValidationStrategy validationStrategy = transitionValidationService.getReviewValidationStrategy(user, notes);
    validationStrategy.validate(kit, kit.getSerialNumber());

    kit.getLegalDetails().setReviewingOrganization(user.getOrganization());
    kit.getLegalDetails().setReviewingProsecutor(user.getDisplayName());
    kit.getLegalDetails().setProsecutorNotes(notes);
    kit.getLegalDetails().setProsecutorAgrees(agree);
    kit.getLegalDetails().setReviewFinalized(LocalDate.now());

    sexualAssaultKitRepository.save(kit);
    applicationEventPublisher.publishEvent(new KitReviewEvent(user, kit));
  }

  private class ChainOfCustodyEventBuilder {
    private EventType eventType;
    private LocalDate eventDate;
    private String notes;
    private OrganizationUser actor;
    private Organization from;
    private Organization to;
    private EventFlag eventFlag;

    public ChainOfCustodyEventBuilder eventType(EventType eventType) {
      this.eventType = eventType;
      return this;
    }

    public ChainOfCustodyEventBuilder eventDate(LocalDate eventDate) {
      this.eventDate = eventDate;
      return this;
    }

    public ChainOfCustodyEventBuilder notes(String notes) {
      this.notes = notes;
      return this;
    }

    public ChainOfCustodyEventBuilder actor(OrganizationUser actor) {
      this.actor = actor;
      return this;
    }

    public ChainOfCustodyEventBuilder from(Organization from) {
      this.from = from;
      return this;
    }

    public ChainOfCustodyEventBuilder to(Organization to) {
      this.to = to;
      return this;
    }

    public ChainOfCustodyEventBuilder eventFlag(EventFlag eventFlag) {
      this.eventFlag = eventFlag;
      return this;
    }

    public ChainOfCustodyEvent build() {
      ChainOfCustodyEvent event = new ChainOfCustodyEvent();
      event.setEventDate(eventDate);
      event.setNotes(notes);
      event.setEventType(eventType);
      event.setDigitalTimestamp(LocalDateTime.now());
      event.setActor(actor.getDisplayName());
      event.setActorOrganization(actor.getOrganization());
      event.setFrom(from);
      event.setTo(to);
      event.setEventFlag(eventFlag);
      return event;
    }
  }
}