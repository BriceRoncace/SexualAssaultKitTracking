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
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.domain.user.organization.LawEnforcementUser;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUserRepository;
import java.util.function.Consumer;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class UnsubscribeNotificationController extends BaseController {
  private final OrganizationUserRepository organizaitonUserRepository;

  public UnsubscribeNotificationController(OrganizationUserRepository organizaitonUserRepository) {
    this.organizaitonUserRepository = organizaitonUserRepository;
  }

  @GetMapping("/unsubscribe/sendUserEmail/{userIdString}")
  public String unsubscribeUserFromSendUserEmail(@PathVariable String userIdString, RedirectAttributes ra) {
    return modifyUser(userIdString, ra, u -> u.setSendUserEmail(false));
  }

  @GetMapping("/unsubscribe/incomingKitEmail/{userIdString}")
  public String unsubscribeUserFromIncomingKitEmail(@PathVariable String userIdString, RedirectAttributes ra) {
    return modifyUser(userIdString, ra, u -> u.setIncomingKitEmail(false));
  }

  @GetMapping("/unsubscribe/attorneyReviewedNotificationEmail/{userIdString}")
  public String unsubscribeUserFromAttorneyReviewedNotificationEmail(@PathVariable String userIdString, RedirectAttributes ra) {
    return modifyUser(userIdString, ra, u -> {
      if (u.getType() == User.Type.LAW_ENFORCEMENT) {
        ((LawEnforcementUser)u).setSendAttorneyReviewedEmail(false);
      }
    });
  }

  private String modifyUser(String userIdString, RedirectAttributes ra, Consumer<AbstractOrganizationUser> consumer) {
    try {
      Long userId = Long.parseLong(userIdString);
      AbstractOrganizationUser user = organizaitonUserRepository.findById(userId).orElse(null);
      if (user == null) {
        ra.addFlashAttribute("errors", getText("unsubscribe.error"));
        return "redirect:/login";
      }

      consumer.accept(user);
      organizaitonUserRepository.save(user);
      return "public/unsubscribe-success";
    }
    catch (NumberFormatException ex) {
      ra.addFlashAttribute("errors", getText("unsubscribe.error"));
      return "redirect:/login";
    }
  }
}
