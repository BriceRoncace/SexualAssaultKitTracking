package gov.idaho.isp.saktrack.controller.user;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUser;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestAttribute;

@Controller
public class ManageAccountController extends BaseController {

  @GetMapping("/manageAccount")
  public String manageAccount(@RequestAttribute OrganizationUser user, Model model) {
    model.addAttribute("orgUser", user);
    return "org-users/save-user";
  }
}