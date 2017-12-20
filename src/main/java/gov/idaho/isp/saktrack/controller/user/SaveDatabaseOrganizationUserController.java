package gov.idaho.isp.saktrack.controller.user;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.service.OrganizationUserPreparer;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.user.persistence.OrganizationUserRepository;
import gov.idaho.isp.saktrack.user.view.DbUserForm;
import gov.idaho.isp.saktrack.util.RoutingUtil;
import java.util.Optional;
import javax.validation.Valid;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class SaveDatabaseOrganizationUserController extends BaseController {
  private final OrganizationUserRepository organizationUserRepository;
  private final OrganizationUserPreparer organizationUserPreparer;
  private final PasswordEncoder passwordEncoder;

  public SaveDatabaseOrganizationUserController(OrganizationUserRepository organizationUserRepository, OrganizationUserPreparer organizationUserPreparer, PasswordEncoder passwordEncoder) {
    this.organizationUserRepository = organizationUserRepository;
    this.organizationUserPreparer = organizationUserPreparer;
    this.passwordEncoder = passwordEncoder;
  }

  @ModelAttribute
  public DbUserForm prepareOrganizationUser(@RequestParam Optional<Long> userId, @RequestParam Long orgId, @RequestParam("passwordPair.oldPassword") Optional<String> oldPassword) {
    return organizationUserPreparer.prepareVerifiedDbUserForm(userId, orgId, oldPassword.isPresent());
  }

  @RequestMapping(value = {"/organization/{orgId}/user/save", "/manageAccount/db"}, method = RequestMethod.POST)
  public String saveDatabaseUser(@Valid DbUserForm userForm, BindingResult br, Model model, RedirectAttributes ra, @RequestAttribute User user) {
    if (br.hasErrors()) {
      model.addAttribute("errors", getErrors(br));
      model.addAttribute("orgUser", userForm.getOrgUser());
      return "org-users/save-user";
    }

    userForm.encodePasswordIfNecessary(passwordEncoder);
    organizationUserRepository.save(userForm.getOrgUser());
    ra.addFlashAttribute("messages", getText("var.save", userForm.getOrgUser().getDisplayName()));
    return RoutingUtil.getManageOrganizationOrDashboardView(user, userForm.getOrgUser().getOrganization());
  }
}
