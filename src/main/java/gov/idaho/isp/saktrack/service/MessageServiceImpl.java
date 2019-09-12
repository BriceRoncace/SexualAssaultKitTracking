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

package gov.idaho.isp.saktrack.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;
import org.springframework.context.MessageSource;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;

@Service
public class MessageServiceImpl implements MessageService {
  private final MessageSource messageSource;

  public MessageServiceImpl(MessageSource messageSource) {
    this.messageSource = messageSource;
  }

  @Override
  public List<String> convertBindingErrors(BindingResult bindingResult) {
    List<String> errors = new ArrayList<>();
    errors.addAll(getFieldBindingFailures(bindingResult));
    errors.addAll(getFieldErrors(bindingResult));
    errors.addAll(getGlobalErrors(bindingResult));
    return errors;
  }

  private List<String> getFieldBindingFailures(BindingResult br) {
    return br.getFieldErrors()
      .stream()
      .filter(fe -> fe.isBindingFailure())
      .map(fe -> messageSource.getMessage(fe, Locale.getDefault()))
      .collect(Collectors.toList());
  }

  private List<String> getFieldErrors(BindingResult br) {
    return br.getFieldErrors()
      .stream()
      .filter(fe -> !fe.isBindingFailure())
      .map(fe -> messageSource.getMessage(fe, Locale.getDefault()))
      .collect(Collectors.toList());
  }

  private List<String> getGlobalErrors(BindingResult br) {
    return br.getGlobalErrors()
      .stream()
      .map(ge -> messageSource.getMessage(ge, Locale.getDefault()))
      .collect(Collectors.toList());
  }

  @Override
  public String getMessageText(String messageName, String... args) {
    return messageSource.getMessage(messageName, args, messageName, Locale.getDefault());
  }

  public MessageSource getMessageSource() {
    return messageSource;
  }
}
