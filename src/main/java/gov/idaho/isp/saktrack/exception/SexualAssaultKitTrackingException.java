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

package gov.idaho.isp.saktrack.exception;

import gov.idaho.isp.saktrack.util.collection.SetUtils;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class SexualAssaultKitTrackingException extends RuntimeException implements ErrorAware {
  private final Set<String> errors;

  public SexualAssaultKitTrackingException(String message, Throwable cause, Set<String> errors) {
    super(message, cause);
    this.errors = errors;
  }

  public SexualAssaultKitTrackingException(String message, Set<String> errors) {
    this(message, null, errors);
  }

  public SexualAssaultKitTrackingException(String message, String... errors) {
    this(message, null, errors);
  }

  public SexualAssaultKitTrackingException(String message, Throwable cause, String... errors) {
    this(message, cause, (errors != null && errors.length > 0 ? SetUtils.of(errors) : SetUtils.of(message)));
  }

  @Override
  public Set<String> getErrors() {
    if (errors == null || errors.isEmpty()) {
      return new HashSet<>(Arrays.asList(getMessage()));
    }
    return errors;
  }
}