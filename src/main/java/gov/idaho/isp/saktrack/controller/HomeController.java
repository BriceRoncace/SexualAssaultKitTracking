package gov.idaho.isp.saktrack.controller;

import gov.idaho.isp.saktrack.user.User;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
public class HomeController extends BaseController {
  
  @RequestMapping(value = "/", method = RequestMethod.GET)
  public String index(@RequestAttribute(required = false) User user) {
    return user != null ? "redirect:/" + user.getType().getLabel() + "/dashboard" : "public/dashboard";
  }
}
