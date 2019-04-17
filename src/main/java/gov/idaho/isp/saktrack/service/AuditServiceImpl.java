package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.domain.HasLabel;
import gov.idaho.isp.saktrack.domain.LabDetails;
import gov.idaho.isp.saktrack.domain.LawEnforcementDetails;
import gov.idaho.isp.saktrack.domain.LegalDetails;
import gov.idaho.isp.saktrack.domain.MedicalDetails;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.audit.KitAudit;
import gov.idaho.isp.saktrack.domain.audit.KitAuditRepository;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.user.User;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class AuditServiceImpl implements AuditService {
  private final String CHANGED = "%1s was changed from \"%2s\" to \"%3s\".";
  private final String ADDED = "Event \"%s\" was added.";
  private final String REMOVED = "Event \"%s\" was removed.";
  private final String MODIFIED = "Event \"%1s\" was change to \"%2s\".";
  private final String MOVED_UP = "Event \"%s\" moved up.";
  private final String MOVED_DOWN = "Event \"%s\" moved down.";

  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final KitAuditRepository kitAuditRepository;
  private final DateTimeFormatter formatter;

  public AuditServiceImpl(SexualAssaultKitRepository sexualAssaultKitRepository, KitAuditRepository kitAuditRepository, @Value("${date.format}") String datePattern) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.kitAuditRepository = kitAuditRepository;
    this.formatter = DateTimeFormatter.ofPattern(datePattern);
  }

  @Override @Transactional
  public KitAudit auditKit(SexualAssaultKit modifiedKit, String notes, User user) {
    if (StringUtils.isBlank(notes)) {
      throw new IllegalArgumentException("Notes/reason must be provided when auditing a sexual assault kit modification.");
    }
    return kitAuditRepository.save(buildKitAudit(modifiedKit, notes, user));
  }

  private KitAudit buildKitAudit(SexualAssaultKit modifiedKit, String notes, User user) {
    SexualAssaultKit existingKit = sexualAssaultKitRepository.findOneInNewTransaction(modifiedKit.getId());

    KitAudit audit = new KitAudit();
    audit.setKitId(modifiedKit.getId());
    audit.setDisplayName(user.getDisplayName());
    audit.setModified(LocalDateTime.now());
    audit.setChanges(getChanges(existingKit, modifiedKit));
    audit.setNotes(notes);
    return audit;
  }

  private List<String> getChanges(SexualAssaultKit oldKit, SexualAssaultKit newKit) {
    List<String> changes = new ArrayList<>();
    changes.add(processItem("Serial Number", oldKit.getSerialNumber() == null ? "" : oldKit.getSerialNumber(), newKit.getSerialNumber() == null ? "" : newKit.getSerialNumber()));
    changes.add(processItem("Expiration Date", getLocalDate(oldKit.getExpirationDate()), getLocalDate(newKit.getExpirationDate())));
    changes.add(processItem("Current Assignment", getOrganizationName(oldKit.getCurrentAssignment()), getOrganizationName(newKit.getCurrentAssignment())));
    changes.add(processItem("Questionable Events", getBoolean(oldKit.getQuestionableEvents()), getBoolean(newKit.getQuestionableEvents())));
    changes.addAll(compareChainOfCustody(oldKit.getChainOfCustody(), newKit.getChainOfCustody()));
    changes.addAll(compareMedicalDetails(oldKit.getMedicalDetails(), newKit.getMedicalDetails()));
    changes.addAll(compareLawEnforcementDetails(oldKit.getLeDetails(), newKit.getLeDetails()));
    changes.addAll(compareLegalDetails(oldKit.getLegalDetails(), newKit.getLegalDetails()));
    changes.addAll(compareLabDetails(oldKit.getLabDetails(), newKit.getLabDetails()));
    changes.removeAll(Collections.singleton(null));
    return changes;
  }

  private List<String> compareLabDetails(LabDetails old, LabDetails edited) {
    LabDetails oldDetails = old == null ? new LabDetails() : old;
    LabDetails newDetails = edited == null ? new LabDetails() : edited;
    List<String> changes = new ArrayList<>();

    changes.add(processItem("Requesting LE Agency (on lab details)", getOrganizationName(oldDetails.getRequestingLeAgency()), getOrganizationName(newDetails.getRequestingLeAgency())));
    changes.add(processItem("Lab Case Number", oldDetails.getCaseNumber(), newDetails.getCaseNumber()));
    changes.add(processItem("Date Completed", getLocalDate(oldDetails.getDateCompleted()), getLocalDate(newDetails.getDateCompleted())));
    changes.add(processItem("DNA Database Entry", getLabel(oldDetails.getDnaDatabaseEntry()), getLabel(newDetails.getDnaDatabaseEntry())));
    changes.add(processItem("DNA Database Hit Date", getLocalDate(oldDetails.getDnaDatabaseHitDate()), getLocalDate(newDetails.getDnaDatabaseHitDate())));
    changes.add(processItem("Expunged Date", getLocalDate(oldDetails.getExpungedDate()), getLocalDate(newDetails.getExpungedDate())));
    return changes;
  }

  private List<String> compareLawEnforcementDetails(LawEnforcementDetails old, LawEnforcementDetails edited) {
    LawEnforcementDetails oldDetails = old == null ? new LawEnforcementDetails() : old;
    LawEnforcementDetails newDetails = edited == null ? new LawEnforcementDetails() : edited;
    List<String> changes = new ArrayList<>();

    changes.add(processItem("Agency Case Number", oldDetails.getCaseNumber(), newDetails.getCaseNumber()));
    changes.add(processItem("Investigator", oldDetails.getInvestigator(), newDetails.getInvestigator()));
    changes.add(processItem("Crime", oldDetails.getCrime(), newDetails.getCrime()));
    changes.add(processItem("Crime Date", getLocalDate(oldDetails.getCrimeDate()), getLocalDate(newDetails.getCrimeDate())));
    changes.add(processItem("Outsourced Lab", oldDetails.getOutsourcedLabName(), newDetails.getOutsourcedLabName()));
    changes.add(processItem("Planned Destruction Date", getLocalDate(oldDetails.getPlannedDestructionDate()), getLocalDate(newDetails.getPlannedDestructionDate())));
    changes.add(processItem("Notify Victim Prior to Destruction", getBoolean(oldDetails.getPlannedDestructionNotificationRequested()), getBoolean(newDetails.getPlannedDestructionNotificationRequested())));
    changes.add(processItem("Meets Submission Criteria", getBoolean(oldDetails.getMeetsSubmissionCriteria()), getBoolean(newDetails.getMeetsSubmissionCriteria())));
    changes.add(processItem("Non-Submission Reason", getLabel(oldDetails.getNonSubmissionReason()), getLabel(newDetails.getNonSubmissionReason())));
    return changes;
  }

  private List<String> compareLegalDetails(LegalDetails old, LegalDetails edited) {
    LegalDetails oldDetails = old == null ? new LegalDetails() : old;
    LegalDetails newDetails = edited == null ? new LegalDetails() : edited;
    List<String> changes = new ArrayList<>();

    changes.add(processItem("Released for Review", getLocalDate(oldDetails.getReleasedForReview()), getLocalDate(newDetails.getReleasedForReview())));
    changes.add(processItem("Reviewing Prosecutor", oldDetails.getReviewingProsecutor(), newDetails.getReviewingProsecutor()));
    changes.add(processItem("Reviewing Prosecutor Organization", getOrganizationName(oldDetails.getReviewingOrganization()), getOrganizationName(newDetails.getReviewingOrganization())));
    changes.add(processItem("Prosecutor Agrees", getBoolean(oldDetails.getProsecutorAgrees()), getBoolean(newDetails.getProsecutorAgrees())));
    changes.add(processItem("Prosecutor Notes", oldDetails.getProsecutorNotes(), newDetails.getProsecutorNotes()));
    changes.add(processItem("Prosecutor Review Finalized", getLocalDate(oldDetails.getReviewFinalized()), getLocalDate(newDetails.getReviewFinalized())));
    return changes;
  }

  private List<String> compareMedicalDetails(MedicalDetails old, MedicalDetails edited) {
    MedicalDetails oldDetails = old == null ? new MedicalDetails() : old;
    MedicalDetails newDetails = edited == null ? new MedicalDetails() : edited;
    List<String> changes = new ArrayList<>();

    changes.add(processItem("Victim Type", getLabel(oldDetails.getVictimType()), getLabel(newDetails.getVictimType())));
    changes.add(processItem("Requesting LE Agency (on medical details)", getOrganizationName(oldDetails.getRequestingLeAgency()), getOrganizationName(newDetails.getRequestingLeAgency())));
    changes.add(processItem("Collection Date", getLocalDate(oldDetails.getCollectionDate()), getLocalDate(newDetails.getCollectionDate())));
    return changes;
  }

  private String processItem(String name, String oldValue, String newValue) {
    String oldValueNullSafe = StringUtils.stripToEmpty(oldValue);
    String newValueNullSafe = StringUtils.stripToEmpty(newValue);
    if (!oldValueNullSafe.equals(newValueNullSafe)) {
      return String.format(CHANGED, name, oldValueNullSafe, newValueNullSafe);
    }
    return null;
  }

  private String getOrganizationName(Organization org) {
    if (org != null) {
      return org.getName();
    }
    return null;
  }

  private String getLabel(HasLabel e) {
    if (e != null) {
      return e.getLabel();
    }
    return null;
  }

  private String getLocalDate(LocalDate date) {
    if (date != null && formatter != null) {
      return date.format(formatter);
    }
    if (date != null) {
      return date.toString();
    }
    return null;
  }

  private String getBoolean(Boolean b) {
    if (b != null) {
      return b.toString();
    }
    return null;
  }

  private List<String> compareChainOfCustody(List<ChainOfCustodyEvent> oldList, List<ChainOfCustodyEvent> newList) {
    List<String> changes = new ArrayList<>();
    for (ChainOfCustodyEvent event : newList) {
      if (!containsSameEntity(oldList, event)) {
        changes.add(String.format(ADDED, event.prettyPrint(true)));
      }
    }
    for (ChainOfCustodyEvent event : oldList) {
      if (!containsSameEntity(newList, event)) {
        changes.add(String.format(REMOVED, event.prettyPrint(true)));
      }
    }
    changes.addAll(getEventDifferences(oldList, newList));
    return changes;
  }

  private List<String> getEventDifferences(List<ChainOfCustodyEvent> oldList, List<ChainOfCustodyEvent> newList) {
    List<String> changes = new ArrayList<>();
    for (int newEventIndex = 0; newEventIndex < newList.size(); newEventIndex++) {
      ChainOfCustodyEvent event = newList.get(newEventIndex);
      if (containsSameEntity(oldList, event)) {
        EventAndIndex existingEventAndIndex = findSameEntity(oldList, event);
        ChainOfCustodyEvent existingEvent = existingEventAndIndex.event;
        int existingEventIndex = existingEventAndIndex.index;

        if (!existingEvent.equals(event)) {
          changes.add(String.format(MODIFIED, existingEvent.prettyPrint(true), event.prettyPrint(true)));
        }

        if (newEventIndex < existingEventIndex) {
          changes.add(String.format(MOVED_UP, existingEvent.prettyPrint(true)));
        }
        else if (newEventIndex > existingEventIndex) {
          changes.add(String.format(MOVED_DOWN, existingEvent.prettyPrint(true)));
        }
      }
    }
    return changes;
  }

  private boolean containsSameEntity(List<ChainOfCustodyEvent> list, ChainOfCustodyEvent event) {
    return list.stream().anyMatch(e -> Objects.equals(e.getId(), event.getId()));
  }

  private EventAndIndex findSameEntity(List<ChainOfCustodyEvent> list, ChainOfCustodyEvent event) {
    List<Long> eventIds = list.stream().map(e -> e.getId()).collect(Collectors.toList());
    int index = eventIds.indexOf(event.getId());
    ChainOfCustodyEvent sameEvent = list.get(index);
    return new EventAndIndex(sameEvent, index);
  }

  private static class EventAndIndex {
    public final ChainOfCustodyEvent event;
    public final int index;

    public EventAndIndex(ChainOfCustodyEvent event, int index) {
      this.event = event;
      this.index = index;
    }
  }
}
