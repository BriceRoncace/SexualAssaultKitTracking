package gov.idaho.isp.saktrack.controller;

import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.util.RoutingUtil;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
public class HelpController {

  @RequestMapping(value = "/help", method = RequestMethod.GET)
  public String help(@RequestAttribute User user) {
    return RoutingUtil.getRoute(user) + "/help";
  }
}