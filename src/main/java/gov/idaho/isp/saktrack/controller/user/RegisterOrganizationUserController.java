package gov.idaho.isp.saktrack.controller.user;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.service.EmailService;
import gov.idaho.isp.saktrack.service.OrganizationUserPreparer;
import gov.idaho.isp.saktrack.user.persistence.OrganizationUserRepository;
import gov.idaho.isp.saktrack.user.view.DbUserForm;
import java.util.Optional;
import javax.validation.Valid;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class RegisterOrganizationUserController extends BaseController {
  private final OrganizationRepository organizationRepository;
  private final OrganizationUserRepository organizationUserRepository;
  private final OrganizationUserPreparer organizationUserPreparer;
  private final PasswordEncoder passwordEncoder;
  private final EmailService emailService;

  public RegisterOrganizationUserController(OrganizationRepository organizationRepository, OrganizationUserRepository organizationUserRepository, OrganizationUserPreparer organizationUserPreparer, PasswordEncoder passwordEncoder, EmailService emailService) {
    this.organizationRepository = organizationRepository;
    this.organizationUserRepository = organizationUserRepository;
    this.organizationUserPreparer = organizationUserPreparer;
    this.passwordEncoder = passwordEncoder;
    this.emailService = emailService;
  }

  @ModelAttribute
  public DbUserForm prepareOrganizationUser(@RequestParam Optional<Long> orgId) {
    return organizationUserPreparer.prepareNewDbUserForm(orgId);
  }

  @RequestMapping(value = "/register", method = RequestMethod.POST)
  public String saveRegisterOrganizationUser(@Valid DbUserForm userForm, BindingResult br, Model model, RedirectAttributes ra) {
    if (br.hasErrors()) {
      model.addAttribute("errors", getErrors(br));
      model.addAttribute("organizations", organizationRepository.findByEnabledOrderByNameAsc(true));
      model.addAttribute("orgUser", userForm.getOrgUser());
      return "/public/user-registration";
    }

    userForm.encodePasswordIfNecessary(passwordEncoder);
    organizationUserRepository.save(userForm.getOrgUser());
    emailService.sendUserRegisteredEmail(userForm.getOrgUser());
    ra.addFlashAttribute("messages", getText("register.success", userForm.getOrgUser().getDisplayName()));
    return "redirect:/login";
  }
}
