package gov.idaho.isp.saktrack.controller;

import gov.idaho.isp.saktrack.event.KitCreateEvent;
import gov.idaho.isp.saktrack.user.organization.OrganizationUser;
import gov.idaho.isp.saktrack.user.persistence.OrganizationUserRepository;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Controller;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class TestController {
  @Autowired
  private OrganizationUserRepository organizationUserRepository;

  @Autowired
  private ApplicationEventPublisher applicationEventPublisher;

  @Autowired
  private BeanFactory beanFactory;

  @RequestMapping(value = "/publishFakeEvent", method = RequestMethod.GET)
  public String publicFakeEvent() {
    System.out.println("Sending event on thread: " + Thread.currentThread());
    applicationEventPublisher.publishEvent(new KitCreateEvent(null, null));
    return "public/login";
  }

  @RequestMapping(value = "/loadUser", method = RequestMethod.GET)
  public String loadUser(@RequestParam Long userId) {

    System.out.println("LabUser bean? " + beanFactory.containsBean("LabUser"));
    System.out.println("labUser bean? " + beanFactory.containsBean("labUser"));
    System.out.println("LabUser bean? " + beanFactory.containsBean("gov.idaho.isp.saktrack.user.organization.LabUser"));


    OrganizationUser user = organizationUserRepository.findOne(userId);
    System.out.println(ReflectionTestUtils.getField(user, "service"));
    System.out.println(ReflectionTestUtils.getField(user, "validationService"));
    return "redirect:/";
  }
}
//
////  @Autowired @Qualifier("upcomingKitDestructionReminder")
////  private UpcomingKitDestructionReminderTask task;
////
////  @RequestMapping(value = "/emailTest", method = RequestMethod.GET)
////  public String emailTest() {
////    task.run();
////    return "redirect:/";
////  }
//
////  @RequestMapping(value = "/saveKits", method = RequestMethod.GET)
////  public String saveKits() {
////    for (SexualAssaultKit kit : repo.findAll()) {
////      kit.setSerialNumber(StringUtils.leftPad(kit.getSerialNumber(), 5, "0"));
////      repo.save(kit);
////    }
////    return "redirect:/";
////  }
////
////
////  @RequestMapping(value = "/saveWidget", method = RequestMethod.GET)
////  public String saveWidget() {
////    SexualAssaultKit kit = new SexualAssaultKit();
////    Set<String> errors = v.validateKit(kit);
////    System.out.println(errors);
////    return "redirect:/";
////  }
////
////@RequestMapping(value = "/dateTest", method = RequestMethod.GET)
////public String dateTest(Model m) {
////  m.addAttribute("today", LocalDate.now());
////  m.addAttribute("todayDateTime", LocalDateTime.now());
////  return "public/dashboard";
////}
////
////@RequestMapping(value = "/test", method = RequestMethod.GET)
////public String test(RedirectAttributes ra) {
////  List<String> messages = Arrays.asList("Message One", "Message Two");
////  ra.addFlashAttribute("messages", messages);
////  return "redirect:/test2";
////}
////
////@RequestMapping(value = "/test2", method = RequestMethod.GET)
////public String test2(ModelMap modelMap) {
////  return "redirect:/test3";
////}
////
////@RequestMapping(value = "/test3", method = RequestMethod.GET)
////public String test3(ModelMap modelMap) {
////  return "public/dashboard";
////}
////
////
////  @RequestMapping(value = "/post", method = RequestMethod.POST)
////  public String post(String value, @RequestParam(defaultValue = "") String value2) {
////    System.out.println("Got value: " + value);
////    System.out.println("Got value: " + value2);
////    return "redirect:/";
////  }
//
//
////  @Autowired
////  private WidgetRepository widgetRepository;
////
////  @ModelAttribute
////  public Widget prepareWidget(@RequestParam Long id) {
////    return widgetRepository.findOne(id);
////  }
////
////  @RequestMapping(value = "/saveWidget", method = RequestMethod.POST)
////  public String saveWidget(Widget widget, BindingResult bindingResult) {
////    Widget existingWidget = widgetRepository.findOneInNewTransaction(widget.getId());
////    System.out.println("Existing name: " + existingWidget.getName());
////    System.out.println("Modified name: " + widget.getName());
////    return "redirect:/";
////  }
////    <form action="<c:url value="/saveWidget"/>" method="POST">
////      <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
////      Id: <input type="text" name="id" value="1"/><br/>
////      Name: <input type="text" name="name" value=""/><br/>
////      <input type="submit"/>
////    </form>
////
////    <hr/>
//}
