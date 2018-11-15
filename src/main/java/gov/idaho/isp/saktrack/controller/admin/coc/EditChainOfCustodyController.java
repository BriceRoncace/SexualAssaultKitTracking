package gov.idaho.isp.saktrack.controller.admin.coc;

import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent.EventType;
import gov.idaho.isp.saktrack.domain.EventFlag;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.service.AuditService;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.util.EventUtil;
import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Collectors;
import javax.validation.Valid;
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
public class EditChainOfCustodyController extends BaseController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final OrganizationRepository organizationRepository;
  private final AuditService auditService;

  public EditChainOfCustodyController(SexualAssaultKitRepository sexualAssaultKitRepository, OrganizationRepository organizationRepository, AuditService auditService) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.organizationRepository = organizationRepository;
    this.auditService = auditService;
  }

  @ModelAttribute
  public void prepareKitAndEvent(@RequestParam Long kitId, @RequestParam Long eventId, @RequestParam Optional<Long> actorOrgId, @RequestParam Optional<Long> fromOrgId, @RequestParam Optional<Long> toOrgId, Model model) {
    SexualAssaultKit kit = sexualAssaultKitRepository.findById(kitId).orElse(null);
    model.addAttribute("kit", kit);

    ChainOfCustodyEvent event = loadEventOrNull(kit, eventId);
    if (event != null) {
      modifyEvent(event, actorOrgId, fromOrgId, toOrgId);
    }
    model.addAttribute("event", event);
  }

  @GetMapping("/admin/editEvent")
  public String editChainOfCustody(@ModelAttribute("kit") SexualAssaultKit kit, @ModelAttribute("event") ChainOfCustodyEvent event, Model model) {
    setupModelForManageEventView(kit, event.getEventType(), model);
    return "admin/manage-events";
  }

  @PostMapping("/admin/updateEvent")
  public String updateChainOfCustody(@ModelAttribute("kit") SexualAssaultKit kit, @Valid @ModelAttribute("event") ChainOfCustodyEvent event, BindingResult br, @RequestParam String reason, @RequestAttribute User user, Model model, RedirectAttributes ra) {
    if (br.hasErrors()) {
      model.addAttribute("errors", getErrors(br));
      setupModelForManageEventView(kit, event.getEventType(), model);
      return "/admin/manage-events";
    }

    kit.setQuestionableEvents(EventUtil.hasMissingSendEvents(kit.getChainOfCustody()));
    auditService.auditKit(kit, reason, user);
    sexualAssaultKitRepository.save(kit);
    ra.addFlashAttribute("messages", getText("var.save", "Chain of custody " + event.getEventType().getLabel() + " event"));
    return "redirect:/admin/editEvent?kitId=" + kit.getId() + "&eventId=" + event.getId();
  }

  @PostMapping("/admin/removeEvent")
  public String removeChainOfCustody(@ModelAttribute("kit") SexualAssaultKit kit, @ModelAttribute("event") ChainOfCustodyEvent event, @RequestParam String reason, @RequestAttribute User user, RedirectAttributes ra) {
    kit.getChainOfCustody().remove(event);
    kit.setQuestionableEvents(EventUtil.hasMissingSendEvents(kit.getChainOfCustody()));
    auditService.auditKit(kit, reason, user);
    sexualAssaultKitRepository.save(kit);
    ra.addFlashAttribute("messages", getText("var.remove", "Chain of custody event "));
    return "redirect:/admin/manageEvents?kitId=" + kit.getId();
  }

  private void setupModelForManageEventView(SexualAssaultKit kit, EventType eventType, Model model) {
    model.addAttribute("missingSendEvents", EventUtil.getMissingSendEvents(kit.getChainOfCustody()));
    model.addAttribute("eventType", eventType);
    model.addAttribute("organizations", organizationRepository.findAll(Sort.by("name")));
    model.addAttribute("sendEventFlags", Arrays.asList(EventFlag.values()).stream().filter(ef -> Boolean.TRUE.equals(ef.isSendEvent())).collect(Collectors.toList()));
    model.addAttribute("receiveEventFlags", Arrays.asList(EventFlag.values()).stream().filter(ef -> Boolean.FALSE.equals(ef.isSendEvent())).collect(Collectors.toList()));
  }

  private ChainOfCustodyEvent loadEventOrNull(SexualAssaultKit kit, Long eventId) {
    return kit.getChainOfCustody().stream().filter(c -> c.getId().equals(eventId)).findFirst().orElse(null);
  }

  private void modifyEvent(ChainOfCustodyEvent event, Optional<Long> actorOrgId, Optional<Long> fromOrgId, Optional<Long> toOrgId) {
    actorOrgId.ifPresent(id -> {
      event.setActorOrganization(organizationRepository.findById(id).orElse(null));
    });

    fromOrgId.ifPresent(id -> {
      event.setFrom(organizationRepository.findById(id).orElse(null));
    });

    toOrgId.ifPresent(id -> {
      event.setTo(organizationRepository.findById(id).orElse(null));
    });

    if (EventType.SEND == event.getEventType()) {
      event.setFrom(event.getActorOrganization());
    }
    else if (EventType.RECEIVE == event.getEventType()) {
      event.setTo(event.getActorOrganization());
    }
    else if (EventType.REPURPOSE == event.getEventType() || EventType.DESTROY == event.getEventType()) {
      event.setFrom(event.getActorOrganization());
    }
  }
}
