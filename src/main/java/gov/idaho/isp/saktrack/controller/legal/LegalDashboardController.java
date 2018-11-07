package gov.idaho.isp.saktrack.controller.legal;

import gov.idaho.isp.saktrack.controller.BaseOrganizationController;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUserRepository;
import gov.idaho.isp.saktrack.util.CookieUtils;
import gov.idaho.isp.saktrack.util.PagingUtils;
import gov.idaho.isp.saktrack.util.SortWrapper;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import javax.servlet.http.HttpServletResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class LegalDashboardController extends BaseOrganizationController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;

  public LegalDashboardController(OrganizationUserRepository organizationUserRepository, SexualAssaultKitRepository sexualAssaultKitRepository) {
    super(organizationUserRepository);
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
  }

  @RequestMapping(value = "/legal/dashboard", method = RequestMethod.GET)
  public String dashboard(@CookieValue Optional<String> legalPagingParams,
                          @RequestAttribute Organization organization,
                          LegalDashboardPageRequest pageRequest,
                          Model model,
                          @RequestAttribute User user,
                          @RequestParam Map<String,String> requestParams,
                          HttpServletResponse response) {

    if (requestParams.isEmpty() && legalPagingParams.isPresent()) {
      return "redirect:/legal/dashboard" + legalPagingParams.get();
    }
    model.addAttribute("organization", organization);

    model.addAttribute("prosecutorKits", findLegalDashboardKits(organization, pageRequest.getPageRequest()));

    if (canAndShouldSeeUnverifiedUserMessage(organization, user)) {
      model.addAttribute("alerts", getText("new.user.verification"));
    }
    response.addCookie(CookieUtils.createEncoded("legalPagingParams", pageRequest.getAsUrlParams()));
    return "legal/dashboard";
  }

  private Page<SexualAssaultKit> findLegalDashboardKits(Organization org, PageRequest pageReq) {
    Function<Pageable,Page<SexualAssaultKit>> getPageFn = pr -> sexualAssaultKitRepository.findByNeedsLegalAttention(org.getId(), org.getJurisdiction(), pr);
    return PagingUtils.getFirstPageIfRequestedIsBlank(getPageFn, pageReq);
  }

  public static class LegalDashboardPageRequest {
    private static final int DEFAULT_PAGE_SIZE = 15;
    private int page;
    private SortWrapper sort;

    public int getPage() {
      return page;
    }

    public void setPage(int page) {
      this.page = page;
    }

    public SortWrapper getSort() {
      return sort;
    }

    public void setSort(SortWrapper sort) {
      this.sort = sort;
    }

    public PageRequest getPageRequest() {
      if (sort != null) {
        return PageRequest.of(page, DEFAULT_PAGE_SIZE, sort.unwrap());
      }
      return PageRequest.of(page, DEFAULT_PAGE_SIZE);
    }

    public String getAsUrlParams() {
      StringBuilder sb = new StringBuilder();
      sb.append("?page=").append(page)
        .append("&sort=").append(PagingUtils.getSingleOrderBy(sort));
      return sb.toString();
    }
  }
}