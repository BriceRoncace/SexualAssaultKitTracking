package gov.idaho.isp.saktrack.service;

import java.util.List;
import org.springframework.validation.BindingResult;

public interface MessageService {
  List<String> convertBindingErrors(BindingResult bindingResult);
  String getMessageText(String messageName, String... args);
}
