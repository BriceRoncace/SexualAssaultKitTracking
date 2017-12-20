package gov.idaho.isp.saktrack.organization;

import java.util.Objects;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Sort;

public class OrganizationSearchCriteria {
  private String name;
  private OrganizationType type;
  private Long jurisdictionId;

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public OrganizationType getType() {
    return type;
  }

  public void setType(OrganizationType type) {
    this.type = type;
  }

  public Long getJurisdictionId() {
    return jurisdictionId;
  }

  public void setJurisdictionId(Long jurisdictionId) {
    this.jurisdictionId = jurisdictionId;
  }

  public boolean isEmpty() {
    return StringUtils.isBlank(name) &&
           type == null &&
           jurisdictionId == null;
  }

  public String buildUrlParams(Page page) {
    StringBuilder sb = new StringBuilder();
    sb.append("?type=").append(Objects.toString(type, ""));
    sb.append("&jurisdictionId=").append(Objects.toString(jurisdictionId, ""));
    sb.append("&name=").append(Objects.toString(name, ""));
    sb.append("&page=").append(page.getNumber());
    sb.append("&sort=").append(getSortParam(page.getSort()));
    return sb.toString();
  }

  private String getSortParam(Sort sort) {
    if (sort == null) {
      return "";
    }

    final StringBuilder sortParam = new StringBuilder();
    sort.forEach(order -> {
      sortParam.append("&sort=").append(order.getProperty()).append(",").append(order.getDirection());
    });

    return sortParam.toString();
  }


  @Override
  public String toString() {
    return "OrganizationSearchCriteria{" + "name=" + name + ", type=" + type + ", jurisdictionId=" + jurisdictionId + '}';
  }
}
