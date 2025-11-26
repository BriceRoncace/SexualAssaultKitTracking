package gov.idaho.isp.saktrack.service;

import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class RangeParserTest {
  private final RangeParser noPrefixParser = new RangeParserImpl(new SerialNumberFormatterImpl("",5));
  private final RangeParser prefixParser = new RangeParserImpl(new SerialNumberFormatterImpl("ABC",5));

  @Test
  public void emptyInput() {
    String input = "";
    List<String> serialNumbers = noPrefixParser.parse(input);
    List<String> expected = new ArrayList<>();

    Assert.assertEquals(expected, serialNumbers);
  }

  @Test
  public void nullInput() {
    List<String> serialNumbers = noPrefixParser.parse(null);
    List<String> expected = new ArrayList<>();

    Assert.assertEquals(expected, serialNumbers);
  }

  @Test
  public void success1() {
    String input = "100-160\n175 176 185-202";
    List<String> serialNumbers = noPrefixParser.parse(input);

    List<String> expected = new ArrayList<>();
    expected.addAll(rangeInclusive(100, 160));
    expected.addAll(rangeInclusive(175, 176));
    expected.addAll(rangeInclusive(185, 202));

    Assert.assertEquals(expected, serialNumbers);
  }

  @Test
  public void success1Prefix() {
    String input = "100-160\n175 176 185-202";
    List<String> serialNumbers = prefixParser.parse(input);

    List<String> expected = new ArrayList<>();
    expected.addAll(rangeInclusive("ABC", 100, 160));
    expected.addAll(rangeInclusive("ABC", 175, 176));
    expected.addAll(rangeInclusive("ABC", 185, 202));

    Assert.assertEquals(expected, serialNumbers);
  }

  @Test
  public void success1Prefix2() {
    String input = "ABC100-ABC160\nABC175 ABC176 ABC185-ABC202";
    List<String> serialNumbers = prefixParser.parse(input);

    List<String> expected = new ArrayList<>();
    expected.addAll(rangeInclusive("ABC", 100, 160));
    expected.addAll(rangeInclusive("ABC", 175, 176));
    expected.addAll(rangeInclusive("ABC", 185, 202));

    Assert.assertEquals(expected, serialNumbers);
  }

  @Test
  public void success2() {
    String input = "100, 103, 105, 107-199";
    List<String> serialNumbers = noPrefixParser.parse(input);

    List<String> expected = new ArrayList<>();
    expected.add("00100");
    expected.add("00103");
    expected.add("00105");
    expected.addAll(rangeInclusive(107, 199));

    Assert.assertEquals(expected, serialNumbers);
  }

  @Test
  public void success2Prefix() {
    String input = "100, 103, 105, 107-199";
    List<String> serialNumbers = prefixParser.parse(input);

    List<String> expected = new ArrayList<>();
    expected.add("ABC00100");
    expected.add("ABC00103");
    expected.add("ABC00105");
    expected.addAll(rangeInclusive("ABC", 107, 199));

    Assert.assertEquals(expected, serialNumbers);
  }

  @Test
  public void success3Prefix() {
    String input = "ABC100, 103, 105, ABC107-199";
    List<String> serialNumbers = prefixParser.parse(input);

    List<String> expected = new ArrayList<>();
    expected.add("ABC00100");
    expected.add("ABC00103");
    expected.add("ABC00105");
    expected.addAll(rangeInclusive("ABC", 107, 199));

    Assert.assertEquals(expected, serialNumbers);
  }

  @Test
  public void success3() {
    String input = "100 103 105, 107-199";
    List<String> serialNumbers = noPrefixParser.parse(input);

    List<String> expected = new ArrayList<>();
    expected.add("00100");
    expected.add("00103");
    expected.add("00105");
    expected.addAll(rangeInclusive(107, 199));

    Assert.assertEquals(expected, serialNumbers);
  }

  @Test
  public void success4() {
    String input = "100 103 105 107-199,200-210,300\n\t310";
    List<String> serialNumbers = noPrefixParser.parse(input);

    List<String> expected = new ArrayList<>();
    expected.add("00100");
    expected.add("00103");
    expected.add("00105");
    expected.addAll(rangeInclusive(107, 199));
    expected.addAll(rangeInclusive(200, 210));
    expected.add("00300");
    expected.add("00310");

    Assert.assertEquals(expected, serialNumbers);
  }

  @Test
  public void overLappingRange() {
    String input = "100-200 150-200";
    List<String> serialNumbers = noPrefixParser.parse(input);

    List<String> expected = new ArrayList<>();
    expected.addAll(rangeInclusive(100, 200));

    Assert.assertEquals(expected, serialNumbers);
  }

  @Test
  public void overLappingRangePrefix() {
    String input = "100-200 150-200";
    List<String> serialNumbers = prefixParser.parse(input);

    List<String> expected = new ArrayList<>();
    expected.addAll(rangeInclusive("ABC", 100, 200));

    Assert.assertEquals(expected, serialNumbers);
  }

  @Test
  public void overLappingRange2() {
    String input = "\n100-150 140-155 410";
    List<String> serialNumbers = noPrefixParser.parse(input);

    List<String> expected = new ArrayList<>();
    expected.addAll(rangeInclusive(100, 155));
    expected.add("00410");

    Assert.assertEquals(expected, serialNumbers);
  }

  @Test
  public void backwardRange() {
    String input = "150-100";
    List<String> serialNumbers = noPrefixParser.parse(input);

    List<String> expected = new ArrayList<>();

    Assert.assertEquals(expected, serialNumbers);
  }

  @Test
  public void rangeOfOne() {
    String input = "150-150";
    List<String> serialNumbers = noPrefixParser.parse(input);

    List<String> expected = new ArrayList<>();
    expected.add("00150");

    Assert.assertEquals(expected, serialNumbers);
  }

  @Test
  public void rangeOfOnePrefix() {
    String input = "150-150";
    List<String> serialNumbers = prefixParser.parse(input);

    List<String> expected = new ArrayList<>();
    expected.add("ABC00150");

    Assert.assertEquals(expected, serialNumbers);
  }

  @Test
  public void rangeIsSequential() {
    String input = "1-10";
    List<String> serialNumbers = noPrefixParser.parse(input);

    List<String> expected = Arrays.asList("00001", "00002", "00003", "00004", "00005", "00006", "00007", "00008", "00009", "00010");
    Assert.assertEquals(expected, serialNumbers);
  }

  @Test
  public void formatTest1() {
    String input = "1";
    List<String> serialNumbers = noPrefixParser.parse(input);

    List<String> expected = Arrays.asList("00001");
    Assert.assertEquals(expected, serialNumbers);
  }

  @Test(expected = IllegalArgumentException.class)
  public void formatTest2() {
    String input = "00000000001";
    List<String> serialNumbers = noPrefixParser.parse(input);
  }

  @Test(expected = IllegalArgumentException.class)
  public void formatTest3() {
    String input = "1234567890";
    List<String> serialNumbers = noPrefixParser.parse(input);
  }

  @Test
  public void formatTest4() {
    String input = "1234";
    List<String> serialNumbers = noPrefixParser.parse(input);

    List<String> expected = Arrays.asList("01234");
    Assert.assertEquals(expected, serialNumbers);
  }

  @Test
  public void formatTest5() {
    String input = "1001";
    List<String> serialNumbers = noPrefixParser.parse(input);

    List<String> expected = Arrays.asList("01001");
    Assert.assertEquals(expected, serialNumbers);
  }

  @Test
  public void formatTest6() {
    String input = "00100";
    List<String> serialNumbers = noPrefixParser.parse(input);

    List<String> expected = Arrays.asList("00100");
    Assert.assertEquals(expected, serialNumbers);
  }

  @Test(expected = IllegalArgumentException.class)
  public void failureDueToRangeWithoutStartValue() {
    String input = "100 103 105, 107 -199";
    List<String> serialNumbers = noPrefixParser.parse(input);
  }

  @Test(expected = IllegalArgumentException.class)
  public void failureDueToRangeWithoutEndValue() {
    String input = "100 103 105, 107- 199";
    List<String> serialNumbers = noPrefixParser.parse(input);
  }

  @Test(expected = IllegalArgumentException.class)
  public void failureDueToRangeWihtMoreThanOneDash() {
    String input = "100 103 105, 107-199-201";
    List<String> serialNumbers = noPrefixParser.parse(input);
  }

  @Test(expected = IllegalArgumentException.class)
  public void failureDueToInvalidSeparator() {
    String input = "100*103";
    List<String> serialNumbers = noPrefixParser.parse(input);
  }

  @Test(expected = IllegalArgumentException.class)
  public void failureDueToInvalidRangeSeparator() {
    String input = "100--103";
    List<String> serialNumbers = noPrefixParser.parse(input);
  }

  @Test(expected = IllegalArgumentException.class)
  public void failureDueToInvalidNumber() {
    String input = "100a 102";
    List<String> serialNumbers = noPrefixParser.parse(input);
  }

  @Test(expected = IllegalArgumentException.class)
  public void failureDueToInvalidNumber2() {
    String input = "ABC100 AB102";
    List<String> serialNumbers = prefixParser.parse(input);
  }

  @Test(expected = IllegalArgumentException.class)
  public void failureDueToInvalidRange() {
    String input = "100 102 -";
    List<String> serialNumbers = noPrefixParser.parse(input);
  }

  private List<String> rangeInclusive(int start, int end) {
    List<String> range = new ArrayList<>();
    for (int i = start; i <= end; i++) {
      range.add(String.format("%05d", i));
    }
    return range;
  }

  private List<String> rangeInclusive(String prefix, int start, int end) {
    List<String> range = new ArrayList<>();
    for (int i = start; i <= end; i++) {
      range.add(String.format(prefix + "%05d", i));
    }
    return range;
  }
}