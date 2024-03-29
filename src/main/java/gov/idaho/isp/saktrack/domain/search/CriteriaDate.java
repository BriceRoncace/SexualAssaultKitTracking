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

package gov.idaho.isp.saktrack.domain.search;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.Expression;
import jakarta.persistence.criteria.Predicate;

public class CriteriaDate {
  public enum SearchType {
    ON("on"),
    AFTER("after"),
    BEFORE("before"),
    BETWEEN("between"),
    NONE("blank");

    private final String label;

    private SearchType (String label) {
      this.label = label;
    }

    public String getLabel() {
      return label;
    }
  }

  private LocalDate date1;
  private LocalDate date2;
  private SearchType searchType = SearchType.ON;

  public LocalDate getDate1() {
    return date1;
  }

  public void setDate1(LocalDate date1) {
    this.date1 = date1;
  }

  public LocalDate getDate2() {
    return date2;
  }

  public void setDate2(LocalDate date2) {
    this.date2 = date2;
  }

  public SearchType getSearchType() {
    return searchType;
  }

  public void setSearchType(SearchType searchType) {
    this.searchType = searchType;
  }

  public String getFilterText(String varName) {
    if (searchType != null) {
      StringBuilder sb = new StringBuilder();
      sb.append(varName).append(" is ").append(searchType.getLabel());

      if (date1 != null) {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("MM/dd/yyyy");
        sb.append(" ");
        if (SearchType.BETWEEN == searchType && date2 != null) {
          sb.append(date1.format(formatter)).append(" and ").append(date2.format(formatter));
        }
        else {
          sb.append(date1.format(formatter));
        }
        return sb.toString();
      }
    }
    return null;
  }

  public boolean canBuildPredicate() {
    if (searchType == SearchType.NONE) {
      return true;
    }

    if (searchType == SearchType.BETWEEN && (date1 == null || date2 == null)) {
      return false;
    }

    return searchType != null && date1 != null;
  }

  public Predicate buildPredicate(CriteriaBuilder cb, Expression exp) {
    Class<?> expType = exp.getJavaType();

    if (searchType == null) {
      throw new IllegalStateException("Cannot build predicate when CriteriaDate searchType is null");
    }
    if (searchType == SearchType.BETWEEN && date1 != null && date2 != null) {
      if (isLocalDateTime(expType)) {
        return cb.between(exp, atStartOfDay(date1), atEndOfDay(date2));
      }
      return cb.between(exp, date1, date2);
    }
    if (searchType == SearchType.ON && date1 != null) {
      if (isLocalDateTime(expType)) {
        return cb.between(exp, atStartOfDay(date1), atEndOfDay(date1));
      }
      return cb.equal(exp, date1);
    }
    if (searchType == SearchType.AFTER && date1 != null) {
      if (isLocalDateTime(expType)) {
        return cb.greaterThan(exp, atEndOfDay(date1));
      }
      return cb.greaterThan(exp, date1);
    }
    if (searchType == SearchType.BEFORE && date1 != null) {
      if (isLocalDateTime(expType)) {
        return cb.lessThan(exp, atStartOfDay(date1));
      }
      return cb.lessThan(exp, date1);
    }
    if (searchType == SearchType.NONE) {
      return cb.isNull(exp);
    }
    return null;
  }

  private boolean isLocalDateTime(Class<?> cls) {
    return LocalDateTime.class.equals(cls);
  }

  private LocalDateTime atStartOfDay(LocalDate ld) {
    return ld.atStartOfDay();
  }

  private LocalDateTime atEndOfDay(LocalDate ld) {
    return ld.atTime(LocalTime.MAX);
  }

  @Override
  public String toString() {
    return "CriteriaDate{" + "date1=" + date1 + ", date2=" + date2 + ", searchType=" + searchType + '}';
  }
}