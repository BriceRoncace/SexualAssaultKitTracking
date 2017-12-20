package gov.idaho.isp.saktrack.controller.interceptor;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.springframework.stereotype.Component;
import org.springframework.ui.ModelMap;
import org.springframework.web.servlet.FlashMap;
import org.springframework.web.servlet.FlashMapManager;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.View;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;
import org.springframework.web.servlet.support.RequestContextUtils;
import org.springframework.web.servlet.view.RedirectView;

/**
 * Converts all model attributes to flash attributes when a redirect occurs.  Flash
 * attributes ensure that the redirected to controller will receive those attributes
 * in the Model (i.e. those attributes will be persisted across the redirect and
 * made available in the Model).
 *
 * Note: this interceptor can be used when controllers are explicitly using
 * RedirectAttributes to store attributes between redirects (in that case the
 * model attributes will be empty and this interceptor will have nothing to convert).
 * The interceptor is useful, however, when a redirect occurs and is handled by a
 * controller method that performs another redirect.  If model attributes are not
 * migrated to flash attributes, they will be lost as request attributes
 * (but preserved as url parameters).
 */
@Component
public class ModelToFlashAttributeInterceptor extends HandlerInterceptorAdapter {
  @Override
  public void postHandle(HttpServletRequest req, HttpServletResponse res, Object handler, ModelAndView modelAndView) throws Exception {
    if (isRedirect(modelAndView)) {
      convertModelAttributesToFlashAttributes(modelAndView.getModelMap(), req, res);
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

  private void convertModelAttributesToFlashAttributes(ModelMap modelMap, HttpServletRequest req, HttpServletResponse res) {
    if (!modelMap.isEmpty()) {
      FlashMapManager flashMapManager = RequestContextUtils.getFlashMapManager(req);
      flashMapManager.saveOutputFlashMap(asFlashMap(modelMap), req, res);
      modelMap.clear();
    }
  }

  private FlashMap asFlashMap(ModelMap modelMap) {
    FlashMap flashMap = new FlashMap();
    flashMap.putAll(modelMap);
    return flashMap;
  }
}