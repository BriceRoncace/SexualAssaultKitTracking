package gov.idaho.isp.saktrack.controller;

import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.util.RoutingUtil;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestAttribute;

@Controller
public class HelpController {

  @GetMapping("/help")
  public String help(@RequestAttribute User user) {
    return RoutingUtil.getRoute(user) + "/help";
  }
}