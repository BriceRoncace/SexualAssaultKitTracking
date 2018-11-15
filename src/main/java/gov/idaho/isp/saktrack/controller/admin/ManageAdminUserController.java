package gov.idaho.isp.saktrack.controller.admin;

import gov.idaho.isp.saktrack.domain.user.AdminUserRepository;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

@Controller
public class ManageAdminUserController {
  private final AdminUserRepository adminUserRepository;

  public ManageAdminUserController(AdminUserRepository adminUserRepository) {
    this.adminUserRepository = adminUserRepository;
  }

  @GetMapping("/adminUser")
  public String newAdminUser(Model model) {
    model.addAttribute("adminUsers", adminUserRepository.findAll(Sort.by("displayName")));
    return "admin/manage-admin";
  }
}
