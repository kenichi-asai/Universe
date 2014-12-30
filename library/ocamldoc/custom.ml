(** language tag: "en" for English, "ja" for Japanese *)
let lang = ref "en"

module Generator (G : Odoc_html.Html_generator) =
struct
  class html =
    object(self)
      inherit G.html

      (** copied from argot: http://argot.x9c.fr *)
      method private string_of_text text =
        let buff = Buffer.create 256 in
        self#html_of_text buff text;
        Buffer.contents buff

      (** Return HTML code for the given text of a lang tag. *)
      method private html_of_lang text = (* your code here *)
        self#string_of_text text

      initializer
        tag_functions <- (!lang, self#html_of_lang) :: tag_functions
        (** register only !lang.  Other tags are ignored *)
  end
end

let _ = begin
  Odoc_args.add_option
    ("-lang", Arg.Set_string lang, "[en|ja]\n\t\tGenerate English document");
  Odoc_args.extend_html_generator (module Generator : Odoc_gen.Html_functor)
end
