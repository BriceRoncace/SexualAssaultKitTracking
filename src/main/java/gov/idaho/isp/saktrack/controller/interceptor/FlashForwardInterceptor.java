package gov.idaho.isp.saktrack.controller.interceptor;

import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.FlashMap;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.View;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;
import org.springframework.web.servlet.support.RequestContextUtils;
import org.springframework.web.servlet.view.RedirectView;

/**
 * Carries forward all flash attributes if the requested view is a redirect.
 * In this way, attributes are maintained across multiple redirects.
 */
@Component
public class FlashForwardInterceptor extends HandlerInterceptorAdapter {
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
