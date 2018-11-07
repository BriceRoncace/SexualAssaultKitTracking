package gov.idaho.isp.saktrack.controller.organization;

import gov.idaho.isp.saktrack.util.PasswordUtil;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class GenerateNewPasskeyRestController {

  @GetMapping("/organization/passkey")
  public String generatePasskey() {
    return PasswordUtil.generatePasskey();
  }
}
