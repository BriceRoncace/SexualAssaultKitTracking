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

package gov.idaho.isp.saktrack.controller.advice;

import gov.idaho.isp.saktrack.controller.advice.LoadEventDetailsAdvice.Apply;
import gov.idaho.isp.saktrack.domain.dto.CreateKitEventDetails;
import gov.idaho.isp.saktrack.domain.dto.EventDetails;
import gov.idaho.isp.saktrack.service.RangeParser;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ModelAttribute;

@ControllerAdvice(annotations = Apply.class)
public class LoadEventDetailsAdvice {

  @Target(ElementType.TYPE)
  @Retention(RetentionPolicy.RUNTIME)
  @Documented
  public @interface Apply {}

  private final RangeParser rangeParser;

  public LoadEventDetailsAdvice(RangeParser rangeParser) {
    this.rangeParser = rangeParser;
  }

  @ModelAttribute
  public CreateKitEventDetails prepareCreateKitEventDetails(CreateKitEventDetails eventDetails, BindingResult ignore) {
    return parseSerialNubers(eventDetails);
  }

  @ModelAttribute
  public EventDetails prepareEventDetails(EventDetails eventDetails, BindingResult ignore) {
    return parseSerialNubers(eventDetails);
  }

  private <T extends EventDetails> T parseSerialNubers(T eventDetails) {
    try {
      eventDetails.setSerialNumberList(rangeParser.parse(eventDetails.getSerialNumbers()));
    }
    catch (Exception ignore) {}
    return eventDetails;
  }
}