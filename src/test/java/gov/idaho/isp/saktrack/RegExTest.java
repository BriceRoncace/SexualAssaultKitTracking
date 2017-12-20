package gov.idaho.isp.saktrack;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.junit.Assert;
import org.junit.Test;

public class RegExTest {
  private static final String DIGITS_OPTIONALLY_SEPARATED_BY_WHITESPACE = "(\\s*?\\d+\\s*?)+?";
  private static final String DIGITS_ONLY = "\\d+";
  private static final String SORT_QUERY_PARAMETER = "([?&]sort=[^&]*|sort=[^&]*?(&|$))";

  @Test
  public void testDigitsOnlyRegEx() {
    Pattern pattern = Pattern.compile(DIGITS_ONLY);

    //Good Input
    Assert.assertTrue(matches(pattern, "123"));

    //Bad Input
    Assert.assertFalse(matches(pattern, ""));
    Assert.assertFalse(matches(pattern, "ABC"));
    Assert.assertFalse(matches(pattern, "123 "));
    Assert.assertFalse(matches(pattern, " 123"));
    Assert.assertFalse(matches(pattern, "L123"));
    Assert.assertFalse(matches(pattern, "123L"));
    Assert.assertFalse(matches(pattern, "1.23"));
    Assert.assertFalse(matches(pattern, "(123)"));
    Assert.assertFalse(matches(pattern, "1-23"));
  }

  @Test
  public void testDigitsOptionallySeparatedByWhitespaceRegEx() {
    Pattern pattern = Pattern.compile(DIGITS_OPTIONALLY_SEPARATED_BY_WHITESPACE);

    // matches (good input)
    Assert.assertTrue(matches(pattern, "123"));
    Assert.assertTrue(matches(pattern, "00123"));
    Assert.assertTrue(matches(pattern, "000000123"));
    Assert.assertTrue(matches(pattern, " 123"));
    Assert.assertTrue(matches(pattern, "123 "));
    Assert.assertTrue(matches(pattern, "123 \n"));
    Assert.assertTrue(matches(pattern, "123 456"));
    Assert.assertTrue(matches(pattern, "123\n456"));
    Assert.assertTrue(matches(pattern, "\t123\n456"));
    Assert.assertTrue(matches(pattern, "123 456 "));
    Assert.assertTrue(matches(pattern, "123 456 \n\n\n"));
    Assert.assertTrue(matches(pattern, "123 46 789"));
    Assert.assertTrue(matches(pattern, "123 456 \n\n\n789"));


    // DOES NOT match (bad input)
    Assert.assertFalse(matches(pattern, ""));
    Assert.assertFalse(matches(pattern, "Abc"));
    Assert.assertFalse(matches(pattern, "123 a56"));
    Assert.assertFalse(matches(pattern, "123 456- 789"));
    Assert.assertFalse(matches(pattern, "123 4.6 789"));
    Assert.assertFalse(matches(pattern, "123 (46) 789"));
  }

  @Test
  public void testSortQueryStringParameter() {
    Pattern pattern = Pattern.compile(SORT_QUERY_PARAMETER);

    Assert.assertTrue(matches(pattern, "sort="));
    Assert.assertTrue(matches(pattern, "?sort="));
    Assert.assertTrue(matches(pattern, "?sort=lastModified"));
    Assert.assertTrue(matches(pattern, "?sort=lastModified,ASC"));
    Assert.assertTrue(matches(pattern, "?sort=labDetails.dateCompleted,DESC"));
    Assert.assertTrue(matches(pattern, "&sort="));
    Assert.assertTrue(matches(pattern, "&sort=lastModified"));
    Assert.assertTrue(matches(pattern, "&sort=lastModified,ASC"));
    Assert.assertTrue(matches(pattern, "&sort=labDetails.dateCompleted,DESC"));

    Assert.assertFalse(matches(pattern, "sort"));
    Assert.assertFalse(matches(pattern, "%sort="));

    //Just in case "sort" is the parameter value and not the name
    Assert.assertFalse(matches(pattern, "=sort&"));

    //Don't chop off next parameter's & sign
    Assert.assertFalse(matches(pattern, "?sort=&"));
    Assert.assertFalse(matches(pattern, "?sort=lastModified&"));
    Assert.assertFalse(matches(pattern, "?sort=lastModified,ASC&"));
    Assert.assertFalse(matches(pattern, "?sort=labDetails.dateCompleted,DESC&"));
    Assert.assertFalse(matches(pattern, "&sort=&"));
    Assert.assertFalse(matches(pattern, "&sort=lastModified&"));
    Assert.assertFalse(matches(pattern, "&sort=lastModified,ASC&"));
    Assert.assertFalse(matches(pattern, "&sort=labDetails.dateCompleted,DESC&"));
  }

  @Test
  public void testStrippingParamFromQueryString() {
    String url1 = "http://server/SexualAssaultKitTracking/search?sort=lastModified,DESC";
    String url2 = "http://server/SexualAssaultKitTracking/search?eventType=SEND&sort=lastModified";
    String url3 = "http://server/SexualAssaultKitTracking/search?eventType=SEND&sort=lastModified&serialNumber=123";
    String url4 = "http://server/SexualAssaultKitTracking/search?way=sort";
    String url5 = "http://server/SexualAssaultKitTracking/search?eventType=SEND&sort=lastModified&sort=dateCompleted";
    String url6 = "http://server/SexualAssaultKitTracking/sort";

    Assert.assertEquals("http://server/SexualAssaultKitTracking/search", url1.replaceAll(SORT_QUERY_PARAMETER, ""));
    Assert.assertEquals("http://server/SexualAssaultKitTracking/search?eventType=SEND", url2.replaceAll(SORT_QUERY_PARAMETER, ""));
    Assert.assertEquals("http://server/SexualAssaultKitTracking/search?eventType=SEND&serialNumber=123", url3.replaceAll(SORT_QUERY_PARAMETER, ""));
    Assert.assertEquals(url4, url4.replaceAll(SORT_QUERY_PARAMETER, ""));
    Assert.assertEquals("http://server/SexualAssaultKitTracking/search?eventType=SEND", url5.replaceAll(SORT_QUERY_PARAMETER, ""));
    Assert.assertEquals(url6, url6.replaceAll(SORT_QUERY_PARAMETER, ""));
  }

  private boolean matches(Pattern pattern, String input) {
    Matcher m = pattern.matcher(input);
    return m.matches();
  }
}
