package gov.idaho.isp.saktrack.controller.admin;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.domain.user.AdminUser;
import gov.idaho.isp.saktrack.domain.user.AdminUserRepository;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.util.UserUtils;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class RemoveAdminUserController extends BaseController {
  private final AdminUserRepository adminUserRepository;

  public RemoveAdminUserController(AdminUserRepository adminUserRepository) {
    this.adminUserRepository = adminUserRepository;
  }

  @PostMapping("/adminUser/remove")
  public String addNewAdminUser(Long userId, RedirectAttributes ra, @RequestAttribute User user) {
    AdminUser admin = adminUserRepository.findById(userId).orElse(null);
    adminUserRepository.delete(admin);
    ra.addFlashAttribute("messages", getText("var.remove", admin.getDisplayName()));
    return UserUtils.isSameUserEntity(admin, user) ? "redirect:/logout" : "redirect:/adminUser";
  }
}
