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

package gov.idaho.isp.saktrack.domain.organization;

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
