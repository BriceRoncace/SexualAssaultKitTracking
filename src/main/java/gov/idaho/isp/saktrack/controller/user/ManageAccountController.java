package gov.idaho.isp.saktrack.controller.user;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.user.organization.OrganizationUser;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
public class ManageAccountController extends BaseController {

  @RequestMapping(value = "/manageAccount", method = RequestMethod.GET)
  public String manageAccount(@RequestAttribute OrganizationUser user, Model model) {
    model.addAttribute("orgUser", user);
    return "org-users/save-user";
  }
}