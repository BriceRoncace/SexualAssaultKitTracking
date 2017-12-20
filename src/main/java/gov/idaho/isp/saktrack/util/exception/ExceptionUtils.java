package gov.idaho.isp.saktrack.util.exception;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

public class ExceptionUtils {

  public static boolean isCause(Class<? extends Throwable> expected, Throwable ex) {
    return expected.isInstance(ex) || (ex != null && isCause(expected, ex.getCause()));
  }

  public static String getRootCauseMessage(Throwable ex) {
    List<Throwable> causes = new ArrayList<>();
    while (ex != null) {
      causes.add(ex);
      ex = ex.getCause();
    }

    Throwable rootCause = causes.get(causes.size()-1);
    return rootCause.getMessage();
  }

  public static String getStackTrace(Throwable exception) {
    if (exception == null) {
      return "";
    }

    StringWriter sw = new StringWriter();
    PrintWriter pw = new PrintWriter(sw);

    try {
      exception.printStackTrace(pw);
      return sw.toString();
    }
    finally {
      try {
        sw.close();
        pw.close();
      }
      catch (IOException ignore) {
      }
    }
  }
}