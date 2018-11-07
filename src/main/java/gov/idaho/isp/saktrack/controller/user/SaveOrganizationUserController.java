package gov.idaho.isp.saktrack.controller.user;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.dto.OrgUserForm;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUserRepository;
import gov.idaho.isp.saktrack.service.UserFormFactory;
import gov.idaho.isp.saktrack.util.RoutingUtil;
import java.util.Optional;
import javax.validation.Valid;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class SaveOrganizationUserController extends BaseController {
  private final OrganizationUserRepository organizationUserRepository;
  private final UserFormFactory userFormFactory;
  private final PasswordEncoder passwordEncoder;

  public SaveOrganizationUserController(OrganizationUserRepository organizationUserRepository, UserFormFactory userFormFactory, PasswordEncoder passwordEncoder) {
    this.organizationUserRepository = organizationUserRepository;
    this.userFormFactory = userFormFactory;
    this.passwordEncoder = passwordEncoder;
  }

  @ModelAttribute
  public OrgUserForm prepareOrganizationUser(@RequestParam Optional<Long> userId, @RequestParam Long orgId, @RequestParam("passwordPair.oldPassword") Optional<String> oldPassword) {
    return userFormFactory.getOrgUserForm(userId, orgId, oldPassword.isPresent());
  }

  @PostMapping({"/organization/{orgId}/user/save", "/manageAccount/db"})
  public String saveDatabaseUser(@Valid OrgUserForm orgUserForm, BindingResult br, Model model, RedirectAttributes ra, @RequestAttribute User user) {
    if (br.hasErrors()) {
      model.addAttribute("errors", getErrors(br));
      model.addAttribute("orgUser", orgUserForm.getOrgUser());
      return "org-users/save-user";
    }

    orgUserForm.encodePasswordIfNecessary(passwordEncoder);
    organizationUserRepository.save(orgUserForm.getOrgUser());
    ra.addFlashAttribute("messages", getText("var.save", orgUserForm.getOrgUser().getDisplayName()));
    return RoutingUtil.getManageOrganizationOrDashboardView(user, orgUserForm.getOrgUser().getOrganization());
  }
}
