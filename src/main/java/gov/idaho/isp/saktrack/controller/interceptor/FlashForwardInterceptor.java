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

package gov.idaho.isp.saktrack.controller.interceptor;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.FlashMap;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.View;
import org.springframework.web.servlet.support.RequestContextUtils;
import org.springframework.web.servlet.view.RedirectView;

import java.util.Map;

/**
 * Carries forward all flash attributes if the requested view is a redirect.
 * In this way, attributes are maintained across multiple redirects.
 */
@Component
public class FlashForwardInterceptor implements HandlerInterceptor {
  @Override
  public void postHandle(HttpServletRequest req, HttpServletResponse res, Object handler, ModelAndView modelAndView) throws Exception {
    if (isRedirect(modelAndView)) {
      carryForwardFlashAttributes(req);
    }
  }

  private boolean isRedirect(ModelAndView modelAndView) {
    if (modelAndView != null) {
      return isRedirectViewInstance(modelAndView.getView()) || isRedirectPrefaced(modelAndView.getViewName());
    }
    return false;
  }

  private boolean isRedirectViewInstance(View view) {
    return view instanceof RedirectView;
  }

  private boolean isRedirectPrefaced(String viewName) {
    return viewName != null && viewName.startsWith("redirect:");
  }

  private void carryForwardFlashAttributes(HttpServletRequest req) {
    Map<String,?> previousFlashAttributes = RequestContextUtils.getInputFlashMap(req);
    FlashMap flashAttributesForNextRequest = RequestContextUtils.getOutputFlashMap(req);
    if (previousFlashAttributes != null && flashAttributesForNextRequest != null) {
      flashAttributesForNextRequest.putAll(previousFlashAttributes);
    }
  }
}
