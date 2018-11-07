package gov.idaho.isp.saktrack.controller;

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUserRepository;
import gov.idaho.isp.saktrack.util.UserUtils;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class ViewChainOfCustodyController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final OrganizationUserRepository organizationUserRepository;

  public ViewChainOfCustodyController(SexualAssaultKitRepository sexualAssaultKitRepository, OrganizationUserRepository organizationUserRepository) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.organizationUserRepository = organizationUserRepository;
  }

  @GetMapping("/timeline")
  public String viewChainOfCustody(@RequestAttribute Optional<User> user, @RequestParam String serialNumber, Model model) {
    if (StringUtils.isNotBlank(serialNumber)) {
      SexualAssaultKit kit = sexualAssaultKitRepository.findBySerialNumber(serialNumber);
      model.addAttribute("kit", kit);
      model.addAttribute("orgContacts", getContacts(kit));
    }
    model.addAttribute("serialNumber", serialNumber);
    return user.isPresent() ? user.get().getType().getLabel() + "/kit-timeline" : "public/kit-timeline";
  }

  private List<AbstractOrganizationUser> getContacts(SexualAssaultKit kit) {
    Long contactOrgId = getContactOrganizationId(kit);
    if (contactOrgId != null) {
      List<AbstractOrganizationUser> contacts = organizationUserRepository.findByOrganizationIdOrderByDisplayNameAsc(contactOrgId);
      return contacts.stream().filter(UserUtils::isContact).collect(Collectors.toList());
    }
    return Collections.emptyList();
  }

  private Long getContactOrganizationId(SexualAssaultKit kit) {
    if (kit == null || kit.getMedicalDetails() == null || kit.getMedicalDetails().getRequestingLeAgency() == null) {
      return null;
    }
    return kit.getMedicalDetails().getRequestingLeAgency().getId();
  }
}
