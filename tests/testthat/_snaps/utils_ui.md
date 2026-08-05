# dl_btn works

    Code
      dl_btn("foo", "Download")
    Output
      <button class="action-button bttn bttn-simple bttn-md bttn-primary bttn-block bttn-no-outline" id="foo_bttn" onclick="getElementById(&#39;foo&#39;).click()" style="background-color: #64C147 !important; border-color: #64C147 !important; color: white !important;" type="button">
        <i class="fas fa-download" role="presentation" aria-label="download icon"></i>
        <a id="foo" class="shiny-download-link" href="" target="_blank" download></a>
        Download
      </button>

---

    Code
      dl_btn("foo", "Download", block = FALSE, size = "sm")
    Output
      <button class="action-button bttn bttn-simple bttn-sm bttn-primary bttn-no-outline" id="foo_bttn" onclick="getElementById(&#39;foo&#39;).click()" style="background-color: #64C147 !important; border-color: #64C147 !important; color: white !important;" type="button">
        <i class="fas fa-download" role="presentation" aria-label="download icon"></i>
        <a id="foo" class="shiny-download-link" href="" target="_blank" download></a>
        Download
      </button>

