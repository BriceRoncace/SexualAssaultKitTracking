package gov.idaho.isp.saktrack.controller.user;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.user.organization.LawEnforcementUser;
import gov.idaho.isp.saktrack.user.persistence.OrganizationUserRepository;
import java.util.function.Consumer;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class UnsubscribeNotificationController extends BaseController {
  private final OrganizationUserRepository organizaitonUserRepository;

  public UnsubscribeNotificationController(OrganizationUserRepository organizaitonUserRepository) {
    this.organizaitonUserRepository = organizaitonUserRepository;
  }

  @RequestMapping(value = "/unsubscribe/sendUserEmail/{userIdString}", method = RequestMethod.GET)
  public String unsubscribeUserFromSendUserEmail(@PathVariable String userIdString, RedirectAttributes ra) {
    return modifyUser(userIdString, ra, u -> u.setSendUserEmail(false));
  }

  @RequestMapping(value = "/unsubscribe/incomingKitEmail/{userIdString}", method = RequestMethod.GET)
  public String unsubscribeUserFromIncomingKitEmail(@PathVariable String userIdString, RedirectAttributes ra) {
    return modifyUser(userIdString, ra, u -> u.setIncomingKitEmail(false));
  }

  @RequestMapping(value = "/unsubscribe/attorneyReviewedNotificationEmail/{userIdString}", method = RequestMethod.GET)
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
      AbstractOrganizationUser user = organizaitonUserRepository.findOne(userId);
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
