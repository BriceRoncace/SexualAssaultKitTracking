package gov.idaho.isp.saktrack.controller.admin.coc;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent.EventType;
import gov.idaho.isp.saktrack.domain.EventFlag;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.service.AuditService;
import gov.idaho.isp.saktrack.service.ValidationService;
import gov.idaho.isp.saktrack.util.EventUtil;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class NewChainOfCustodyController extends BaseController  {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final OrganizationRepository organizationRepository;
  private final ValidationService validationService;
  private final AuditService auditService;

  public NewChainOfCustodyController(SexualAssaultKitRepository sexualAssaultKitRepository, OrganizationRepository organizationRepository, ValidationService validationService, AuditService auditService) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.organizationRepository = organizationRepository;
    this.validationService = validationService;
    this.auditService = auditService;
  }

  @ModelAttribute
  public void prepareKitAndEvent(@RequestParam Long kitId, @RequestParam Optional<Long> actorOrgId, @RequestParam Optional<Long> fromOrgId, @RequestParam Optional<Long> toOrgId, Model model) {
    SexualAssaultKit kit = sexualAssaultKitRepository.findById(kitId).orElse(null);
    model.addAttribute("kit", kit);

    ChainOfCustodyEvent event = new ChainOfCustodyEvent();
    actorOrgId.ifPresent(id -> {
      event.setActorOrganization(organizationRepository.findById(id).orElse(null));
    });
    fromOrgId.ifPresent(id -> {
      event.setFrom(organizationRepository.findById(id).orElse(null));
    });
    toOrgId.ifPresent(id -> {
      event.setTo(organizationRepository.findById(id).orElse(null));
    });
    model.addAttribute("event", event);
  }

  @GetMapping("/admin/enterEvent")
  public String enterChainOfCustodyEvent(@ModelAttribute("kit") SexualAssaultKit kit, @RequestParam EventType eventType, Model model) {
    setupModelForManageEventView(kit, eventType, model);
    return "/admin/manage-events";
  }

  @PostMapping("/admin/newEvent")
  public String newChainOfCustodyEvent(@ModelAttribute("kit") SexualAssaultKit kit, @ModelAttribute("event") ChainOfCustodyEvent event, BindingResult br, @RequestParam String reason, @RequestAttribute User user, Model model, RedirectAttributes ra) {
    setAdditionalEventDetails(event);

    Set<String> errors = validationService.valiateChainOfCustodyEvent(event);
    if (!errors.isEmpty()) {
      model.addAttribute("errors", errors);
      setupModelForManageEventView(kit, event.getEventType(), model);
      return "/admin/manage-events";
    }

    kit.addChainOfCustodyEvent(event);
    kit.setQuestionableEvents(EventUtil.hasMissingSendEvents(kit.getChainOfCustody()));
    auditService.auditKit(kit, reason, user);
    sexualAssaultKitRepository.save(kit);
    ra.addFlashAttribute("messages", getText("var.save", "Chain of custody " + event.getEventType().getLabel() + " event"));
    return "redirect:/admin/manageEvents?kitId=" + kit.getId();
  }

  private void setupModelForManageEventView(SexualAssaultKit kit, EventType eventType, Model model) {
    model.addAttribute("missingSendEvents", EventUtil.getMissingSendEvents(kit.getChainOfCustody()));
    model.addAttribute("eventType", eventType);
    model.addAttribute("organizations", organizationRepository.findAll(new Sort("name")));
    model.addAttribute("sendEventFlags", Arrays.asList(EventFlag.values()).stream().filter(ef -> Boolean.TRUE.equals(ef.isSendEvent())).collect(Collectors.toList()));
    model.addAttribute("receiveEventFlags", Arrays.asList(EventFlag.values()).stream().filter(ef -> Boolean.FALSE.equals(ef.isSendEvent())).collect(Collectors.toList()));
  }

  private void setAdditionalEventDetails(ChainOfCustodyEvent event) {
    event.setDigitalTimestamp(LocalDateTime.now());
    if (ChainOfCustodyEvent.EventType.SEND == event.getEventType()) {
      event.setFrom(event.getActorOrganization());
    }
    else if (ChainOfCustodyEvent.EventType.RECEIVE == event.getEventType()) {
      event.setTo(event.getActorOrganization());
    }
    else if (EventType.REPURPOSE == event.getEventType() || EventType.DESTROY == event.getEventType()) {
      event.setFrom(event.getActorOrganization());
    }
  }
}
