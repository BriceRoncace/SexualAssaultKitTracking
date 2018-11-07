package gov.idaho.isp.saktrack.controller;

import gov.idaho.isp.saktrack.domain.user.User;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestAttribute;

@Controller
public class HomeController extends BaseController {

  @GetMapping("/")
  public String index(@RequestAttribute(required = false) User user) {
    return user != null ? "redirect:/" + user.getType().getLabel() + "/dashboard" : "public/dashboard";
  }
}
