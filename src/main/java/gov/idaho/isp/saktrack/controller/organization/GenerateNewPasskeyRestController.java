package gov.idaho.isp.saktrack.controller.organization;

import gov.idaho.isp.saktrack.util.PasswordUtil;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class GenerateNewPasskeyRestController {

  @RequestMapping(value="/organization/passkey", method = RequestMethod.GET)
  public String generatePasskey() {
    return PasswordUtil.generatePasskey();
  }
}
