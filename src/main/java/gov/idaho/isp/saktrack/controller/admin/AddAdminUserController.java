package gov.idaho.isp.saktrack.controller.admin;

import gov.idaho.isp.ldap.user.LdapUser;
import gov.idaho.isp.ldap.user.LdapUserDirectory;
import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.user.AdminUser;
import gov.idaho.isp.saktrack.user.persistence.AdminUserRepository;
import javax.validation.Valid;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class AddAdminUserController extends BaseController {
  private final LdapUserDirectory ldapUserDirectory;
  private final AdminUserRepository adminUserRepository;

  public AddAdminUserController(LdapUserDirectory ldapUserDirectory, AdminUserRepository adminUserRepository) {
    this.ldapUserDirectory = ldapUserDirectory;
    this.adminUserRepository = adminUserRepository;
  }

  @ModelAttribute
  public AdminUser prepareAdminUser(@RequestParam String ldapUsername) {
    LdapUser ldapUser = ldapUserDirectory.loadByUsername(ldapUsername);
    return ldapUser != null ? new AdminUser(ldapUser) : new AdminUser();
  }

  @RequestMapping(value = "/adminUser/new", method = RequestMethod.POST)
  public String addNewAdminUser(@Valid AdminUser adminUser, BindingResult br, RedirectAttributes ra) {
    if (br.hasErrors()) {
      ra.addFlashAttribute("errors", getErrors(br));
      return "redirect:/adminUser";
    }

    adminUserRepository.save(adminUser);
    ra.addFlashAttribute("messages", getText("var.save", adminUser.getDisplayName()));
    return "redirect:/adminUser";
  }
}
