package gov.idaho.isp.saktrack.util.web;

import gov.idaho.isp.saktrack.util.exception.ExceptionUtils;
import java.util.Date;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;

public class WebExceptionUtils {

  public static String getExceptionMessage(HttpServletRequest req, Throwable exception) {
    StringBuilder errorMsg = new StringBuilder();
    errorMsg.append("User: ").append(req.getRemoteUser()).append(" (Host: ").append(req.getRemoteHost()).append(" IP: ").append(req.getRemoteAddr()).append(")\n\n");
    errorMsg.append("Time: ").append(new Date()).append("\n\n");
    errorMsg.append("Url: ").append(req.getRequestURL()).append("\n\n");
    errorMsg.append("Query String: ").append(req.getQueryString()).append("\n\n");
    errorMsg.append(getRequestParameters(req)).append("\n\n");
    errorMsg.append(getRequestHeaders(req)).append("\n\n");
    if (exception != null) {
      errorMsg.append("Stack Trace:\n").append(ExceptionUtils.getStackTrace(exception));
    }

    return errorMsg.toString();
  }

  public static StringBuilder getRequestParameters(HttpServletRequest req) {
    StringBuilder params = new StringBuilder();
    params.append("Request Parameters:");
    for (Iterator i = req.getParameterMap().entrySet().iterator(); i.hasNext();) {
      Map.Entry entry = (Map.Entry) i.next();

      String[] values = (String[]) entry.getValue();
      for (int x = 0; x < values.length; x++) {
        params.append("\n\t");
        params.append(entry.getKey());
        params.append(": ");
        params.append(values[x]);
      }
    }
    return params;
  }

  public static StringBuilder getRequestHeaders(HttpServletRequest req) {
    StringBuilder headers = new StringBuilder("HTTP Headers:");
    Enumeration e = req.getHeaderNames();
    while (e.hasMoreElements()) {
      String headerName = (String) e.nextElement();
      headers.append("\n\t");
      headers.append(headerName).append(": ").append(req.getHeader(headerName));
    }
    return headers;
  }
}