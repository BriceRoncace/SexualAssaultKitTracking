package gov.idaho.isp.saktrack.controller.lab;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.controller.advice.LoadEventDetailsAdvice.LoadEventDetails;
import gov.idaho.isp.saktrack.domain.dto.CreateKitEventDetails;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.util.UserUtils;
import gov.idaho.isp.saktrack.domain.user.organization.LabUser;
import gov.idaho.isp.saktrack.validation.group.Batch;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
@LoadEventDetails
public class LabCreateKitController extends BaseController {

  @ModelAttribute
  public LabUser prepareLoggedInUser(@RequestAttribute User user) {
    UserUtils.verifyUserTypeOrThrowException(user, User.Type.LAB);
    return (LabUser) user;
  }

  @PostMapping("/lab/createKit")
  public String createKit(LabUser labUser, @Validated(Batch.class) CreateKitEventDetails eventDetails, BindingResult br, RedirectAttributes ra) {
    if (br.hasErrors()) {
      ra.addFlashAttribute("errors", getErrors(br));
      return "redirect:/lab/dashboard";
    }

    try {
      labUser.batchCreate(eventDetails);
      ra.addFlashAttribute("messages", getText("kits.transaction", eventDetails.getSerialNumbersForMessage(), "created"));
    }
    catch (IllegalStateException ex) {
      ra.addFlashAttribute("errors", ex.getMessage());
    }
    catch (DataIntegrityViolationException ex) {
      ra.addFlashAttribute("errors", getText("kit", eventDetails.getSerialNumbersForMessage(), "already exists"));
    }
    return "redirect:/lab/dashboard";
  }

  @PostMapping("/lab/createKitCount")
  public @ResponseBody Integer returnSerialNumberCount(@Validated(Batch.class) CreateKitEventDetails eventDetails, BindingResult br) {
    return br.hasErrors() ? 0 : eventDetails.getSerialNumberList().size();
  }
}
