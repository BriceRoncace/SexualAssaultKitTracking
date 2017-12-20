package gov.idaho.isp.saktrack.controller;

import gov.idaho.isp.saktrack.service.MessageService;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.BindingResult;

public abstract class BaseController {
  private MessageService messageService;

  protected List<String> getErrors(BindingResult... bindingResult) {
    return Stream.of(bindingResult).map(messageService::convertBindingErrors).flatMap(l -> l.stream()).collect(Collectors.toList());
  }

  protected String getText(String text, String... args) {
    return messageService.getMessageText(text, args);
  }

  @Autowired
  public void setMessageService(MessageService messageService) {
    this.messageService = messageService;
  }
}
