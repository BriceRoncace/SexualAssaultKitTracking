package gov.idaho.isp.saktrack.controller.organization;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.persistence.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.persistence.search.SexualAssaultKitSearchCriteria;
import gov.idaho.isp.saktrack.persistence.search.SexualAssaultKitSpec;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.user.persistence.OrganizationUserRepository;
import java.util.List;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class RemoveOrganizationContoller extends BaseController {
  private final OrganizationRepository organizationRepository;
  private final OrganizationUserRepository organizationUserRepository;
  private final SexualAssaultKitRepository sexualAssaultKitRepository;

  public RemoveOrganizationContoller(OrganizationRepository organizationRepository, OrganizationUserRepository organizationUserRepository, SexualAssaultKitRepository sexualAssaultKitRepository) {
    this.organizationRepository = organizationRepository;
    this.organizationUserRepository = organizationUserRepository;
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
  }

  @RequestMapping(value = "/organization/{orgId}/remove", method = RequestMethod.POST)
  public String removeOrganization(@PathVariable Long orgId, RedirectAttributes ra, @RequestAttribute User user) {
    Organization org = organizationRepository.findOne(orgId);
    if (isOrganizationAssociatedToAnyKits(org)) {
      ra.addFlashAttribute("errors", String.format("%s cannot be removed because it is associated to sexual assault kits.", org.getName()));
      return "redirect:/";
    }

    removeAllUsers(org);
    removeOrganization(org);
    ra.addFlashAttribute("messages", getText("var.remove", org.getName()));
    return "redirect:/";
  }

  private boolean isOrganizationAssociatedToAnyKits(Organization org) {
    return isOrganizationInAnyKitChainOfCustody(org) || hasOrganizationReviewedAnyKit(org);
  }

  private boolean isOrganizationInAnyKitChainOfCustody(Organization org) {
    SexualAssaultKitSearchCriteria criteria = new SexualAssaultKitSearchCriteria();
    criteria.setEventOrganization(org.getId());
    return sexualAssaultKitRepository.count(new SexualAssaultKitSpec(criteria)) > 0;
  }

  private boolean hasOrganizationReviewedAnyKit(Organization org) {
    SexualAssaultKitSearchCriteria criteria = new SexualAssaultKitSearchCriteria();
    criteria.setReviewingProsecutorOrganization(org.getId());
    return sexualAssaultKitRepository.count(new SexualAssaultKitSpec(criteria)) > 0;
  }

  private void removeAllUsers(Organization org) {
    List<AbstractOrganizationUser> users = organizationUserRepository.findByOrganizationIdOrderByDisplayNameAsc(org.getId());
    organizationUserRepository.delete(users);
  }

  private void removeOrganization(Organization org) {
    organizationRepository.delete(org);
  }
}
