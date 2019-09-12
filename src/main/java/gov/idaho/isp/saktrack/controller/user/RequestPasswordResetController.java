/* 
 * Copyright 2017 Idaho State Police.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package gov.idaho.isp.saktrack.controller.user;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.service.PasswordResetService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class RequestPasswordResetController extends BaseController {
  private final PasswordResetService passwordResetService;

  public RequestPasswordResetController(PasswordResetService passwordResetService) {
    this.passwordResetService = passwordResetService;
  }

  @PostMapping("/reset/request")
  public String requestPasswordReset(@RequestParam String username, RedirectAttributes redirectAttributes) {
    String error = passwordResetService.requestReset(username);
    if (StringUtils.isNotBlank(error)) {
      redirectAttributes.addFlashAttribute("errors", getText(error));
    }
    else {
      redirectAttributes.addFlashAttribute("messages", getText("password.reset.email.success"));
    }
    return "redirect:/login";
  }
}
