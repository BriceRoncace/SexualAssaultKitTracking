package gov.idaho.isp.saktrack.controller.admin;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.user.AdminUser;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.user.UserUtils;
import gov.idaho.isp.saktrack.user.persistence.AdminUserRepository;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class RemoveAdminUserController extends BaseController {
  private final AdminUserRepository adminUserRepository;

  public RemoveAdminUserController(AdminUserRepository adminUserRepository) {
    this.adminUserRepository = adminUserRepository;
  }

  @RequestMapping(value = "/adminUser/remove", method = RequestMethod.POST)
  public String addNewAdminUser(Long userId, RedirectAttributes ra, @RequestAttribute User user) {
    AdminUser admin = adminUserRepository.findOne(userId);
    adminUserRepository.delete(admin);
    ra.addFlashAttribute("messages", getText("var.remove", admin.getDisplayName()));
    return UserUtils.isSameUserEntity(admin, user) ? "redirect:/logout" : "redirect:/adminUser";
  }
}
