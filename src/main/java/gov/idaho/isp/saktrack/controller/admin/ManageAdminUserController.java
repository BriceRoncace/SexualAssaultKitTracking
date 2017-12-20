package gov.idaho.isp.saktrack.controller.admin;

import gov.idaho.isp.saktrack.user.persistence.AdminUserRepository;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
public class ManageAdminUserController {
  private final AdminUserRepository adminUserRepository;

  public ManageAdminUserController(AdminUserRepository adminUserRepository) {
    this.adminUserRepository = adminUserRepository;
  }

  @RequestMapping(value = "/adminUser", method = RequestMethod.GET)
  public String newAdminUser(Model model) {
    model.addAttribute("adminUsers", adminUserRepository.findAll(new Sort("displayName")));
    return "admin/manage-admin";
  }
}
