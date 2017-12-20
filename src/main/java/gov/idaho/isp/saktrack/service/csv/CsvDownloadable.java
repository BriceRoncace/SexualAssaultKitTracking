package gov.idaho.isp.saktrack.service.csv;

import java.nio.charset.Charset;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;

public class CsvDownloadable {
  public static final MediaType MEDIA_TYPE = new MediaType("text", "csv", Charset.forName("utf-8"));

  private final String fileName;
  private final byte[] content;

  public CsvDownloadable(String fileName, byte[] content) {
    this.fileName = fileName;
    this.content = content;
  }

  public String getFileName() {
    return fileName;
  }

  public byte[] getContent() {
    return content;
  }

  public int getLength() {
    return content == null ? 0 : content.length;
  }

  public HttpEntity<byte[]> toHttpEntity() {
    HttpHeaders header = new HttpHeaders();
    header.setContentType(MEDIA_TYPE);
    header.set(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + getFileName());
    header.setContentLength(getLength());
    return new HttpEntity<>(getContent(), header);
  }
}