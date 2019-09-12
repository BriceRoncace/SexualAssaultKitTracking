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

package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.domain.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.domain.jurisdiction.JurisdictionRepository;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.search.SexualAssaultKitSearchCriteria;
import java.util.StringJoiner;
import org.springframework.stereotype.Service;

@Service
public class FilterTextServiceImpl implements FilterTextService {
  private final OrganizationRepository organizationRepository;
  private final JurisdictionRepository jurisdictionRepository;

  public FilterTextServiceImpl(OrganizationRepository organizationRepository, JurisdictionRepository jurisdictionRepository) {
    this.organizationRepository = organizationRepository;
    this.jurisdictionRepository = jurisdictionRepository;
  }

  @Override
  public String buildReportFilterText(SexualAssaultKitSearchCriteria criteria) {
    StringJoiner sj = new StringJoiner(" | ");

    if (criteria.getCollectedDate() != null) {
      String dateFilterText = criteria.getCollectedDate().getFilterText("Collected Date");
      if (dateFilterText != null) {
        sj.add(dateFilterText);
      }
    }

    if (criteria.getJurisdictionId() != null) {
      Jurisdiction j = jurisdictionRepository.findById(criteria.getJurisdictionId()).orElse(null);
      sj.add("Jurisdiction is " + j.getDisplayName());
    }

    if (criteria.getRequestingLeAgencyId() != null) {
      Organization org = organizationRepository.findById(criteria.getRequestingLeAgencyId()).orElse(null);
      sj.add("Requesting Agency is " + org.getName());
    }

    if (criteria.getCurrentJurisdictionId() != null) {
      Jurisdiction j = jurisdictionRepository.findById(criteria.getCurrentJurisdictionId()).orElse(null);
      sj.add("Jurisdiction is " + j.getDisplayName());
    }

    if (criteria.getCurrentAgencyId() != null) {
      Organization org = organizationRepository.findById(criteria.getCurrentAgencyId()).orElse(null);
      sj.add("Organization is " + org.getName());
    }

    return sj.toString();
  }
}
