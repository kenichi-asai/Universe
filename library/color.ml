(* $B;29M$K$7$?%5%$%H"*(Bhttp://lowlife.jp/yasusii/static/color_chart.html *)

(* $B>e$N%5%$%H$K$"$k?'$r$=$N$^$^$9$Y$F$b$C$F$-$F$$$k$N$G!"(B
$B!!(B $B%5%$%H$K5-:\$5$l$F$$$k?'L>$rF~NO$9$l$P$=$N$^$^;H$($^$9!#(B
  $B"($?$@$7(Bocaml$B$N;EMM>e!"#1J8;zL\$O>.J8;z$KJQ$($F$$$k$N$GCm0U(B *)
(* $BNI$1$l$P;H$C$F$/$@$5$$$J(B *)

(* $B?'(B *)
type t = int32

let asciiA_10 = Char.code 'A' - 10

let rec to_hex n =      (* $B#1#6?JJ8;zNs$KJQ49$9$k(B *)
  let ten = (n mod 256) / 16 in
  let one = n mod 16 in
  (if ten < 10 then string_of_int ten
               else String.make 1 (Char.chr (asciiA_10 + ten))) ^
  (if one < 10 then string_of_int one
               else String.make 1 (Char.chr (asciiA_10 + one)))

(* $BF)2aN($N=i4|CM$O(B255 *)
let make_color ?(alpha = 255) r g b =
  Int32.add (Int32.mul (Int32.of_int ((r * 256 + g) * 256 + b))
                       (Int32.of_int 256))
            (Int32.of_int alpha)

(* convert a color into RGBA *)
let to_rgba rgba =
  let int255 = Int32.of_int 255 in
  let a = Int32.to_int (Int32.logand rgba int255) in
  let rgb = Int32.shift_right_logical rgba 8 in
  let b = Int32.to_int (Int32.logand rgb int255) in
  let rg = Int32.shift_right_logical rgb 8 in
  let g = Int32.to_int (Int32.logand rg int255) in
  let r = Int32.to_int (Int32.shift_right_logical rg 8) in
  (r, g, b, a)

(* convert a color into RGB ignoring alpha *)
let to_rgb color =
  let (r, g, b, a) = to_rgba color in
  (r, g, b)

(* convert a color into int32 *)
let to_int32 color = color

(* These colors : Color.t *)
let transparent         = make_color 255 255 255 ~alpha:0
let snow                = make_color 255 250 250
let ghostWhite          = make_color 248 248 255
let whiteSmoke          = make_color 245 245 245
let gainsboro           = make_color 220 220 220
let floralWhite         = make_color 255 250 240
let oldLace             = make_color 253 245 230
let linen               = make_color 250 240 230
let antiqueWhite        = make_color 250 235 215
let papayaWhip          = make_color 255 239 213
let blanchedAlmond      = make_color 255 235 205
let bisque              = make_color 255 228 196
let peachPuff           = make_color 255 218 185
let navajoWhite         = make_color 255 222 173
let moccasin            = make_color 255 228 181
let cornsilk            = make_color 255 248 220
let ivory               = make_color 255 255 240
let lemonChiffon        = make_color 255 250 205
let seashell            = make_color 255 245 238
let honeydew            = make_color 240 255 240
let mintCream           = make_color 245 255 250
let azure               = make_color 240 255 255
let aliceBlue           = make_color 240 248 255
let lavender            = make_color 230 230 250
let lavenderBlush       = make_color 255 240 245
let mistyRose           = make_color 255 228 225
let white               = make_color 255 255 255
let black               = make_color 0 0 0
let darkSlateGray       = make_color 47 79 79
let dimGray             = make_color 105 105 105
let slateGray           = make_color 112 128 144
let lightSlateGray      = make_color 119 136 153
let gray                = make_color 190 190 190
let lightGray           = make_color 211 211 211
let midnightBlue        = make_color 25 25 112
let navy                = make_color 0 0 128
let navyBlue            = make_color 0 0 128
let cornflowerBlue      = make_color 100 149 237
let darkSlateBlue       = make_color 72 61 139
let slateBlue           = make_color 106 90 205
let mediumSlateBlue     = make_color 123 104 238
let lightSlateBlue      = make_color 132 112 255
let mediumBlue          = make_color 0 0 205
let royalBlue           = make_color 65 105 225
let blue                = make_color 0 0 255
let dodgerBlue          = make_color 30 144 255
let deepSkyBlue         = make_color 0 191 255
let skyBlue             = make_color 135 206 235
let lightSkyBlue        = make_color 135 206 250
let steelBlue           = make_color 70 130 180
let lightSteelBlue      = make_color 176 196 222
let lightBlue           = make_color 173 216 230
let powderBlue          = make_color 176 224 230
let paleTurquoise       = make_color 175 238 238
let darkTurquoise       = make_color 0 206 209
let mediumTurquoise     = make_color 72 209 204
let turquoise           = make_color 64 224 208
let cyan                = make_color 0 255 255
let lightCyan           = make_color 224 255 255
let cadetBlue           = make_color 95 158 160
let mediumAquamarine    = make_color 102 205 170
let aquamarine          = make_color 127 255 212
let darkGreen           = make_color 0 100 0
let darkOliveGreen      = make_color 85 107 47
let darkSeaGreen        = make_color 143 188 143
let seaGreen            = make_color 46 139 87
let mediumSeaGreen      = make_color 60 179 113
let lightSeaGreen       = make_color 32 178 170
let paleGreen           = make_color 152 251 152
let springGreen         = make_color 0 255 127
let lawnGreen           = make_color 124 252 0
let green               = make_color 0 255 0
let chartreuse          = make_color 127 255 0
let mediumSpringGreen   = make_color 0 250 154
let greenYellow         = make_color 173 255 47
let limeGreen           = make_color 50 205 50
let yellowGreen         = make_color 154 205 50
let forestGreen         = make_color 34 139 34
let oliveDrab           = make_color 107 142 35
let darkKhaki           = make_color 189 183 107
let khaki               = make_color 240 230 140
let paleGoldenrod       = make_color 238 232 170
let lightGoldenrodYellow= make_color 250 250 210
let lightYellow         = make_color 255 255 224
let yellow              = make_color 255 255 0
let gold                = make_color 255 215 0
let lightGoldenrod      = make_color 238 221 130
let goldenrod           = make_color 218 165 32
let darkGoldenrod       = make_color 184 134 11
let rosyBrown           = make_color 188 143 143
let indianRed           = make_color 205 92 92
let saddleBrown         = make_color 139 69 19
let sienna              = make_color 160 82 45
let peru                = make_color 205 133 63
let burlywood           = make_color 222 184 135
let beige               = make_color 245 245 220
let wheat               = make_color 245 222 179
let sandyBrown          = make_color 244 164 96
let tan                 = make_color 210 180 140
let chocolate           = make_color 210 105 30
let firebrick           = make_color 178 34 34
let brown               = make_color 165 42 42
let darkSalmon          = make_color 233 150 122
let salmon              = make_color 250 128 114
let lightSalmon         = make_color 255 160 122
let orange              = make_color 255 165 0
let darkOrange          = make_color 255 140 0
let coral               = make_color 255 127 80
let lightCoral          = make_color 240 128 128
let tomato              = make_color 255 99 71
let orangeRed           = make_color 255 69 0
let red                 = make_color 255 0 0
let hotPink             = make_color 255 105 180
let deepPink            = make_color 255 20 147
let pink                = make_color 255 192 203
let lightPink           = make_color 255 182 193
let paleVioletRed       = make_color 219 112 147
let maroon              = make_color 176 48 96
let mediumVioletRed     = make_color 199 21 133
let violetRed           = make_color 208 32 144
let magenta             = make_color 255 0 255
let violet              = make_color 238 130 238
let plum                = make_color 221 160 221
let orchid              = make_color 218 112 214
let mediumOrchid        = make_color 186 85 211
let darkOrchid          = make_color 153 50 204
let darkViolet          = make_color 148 0 211
let blueViolet          = make_color 138 43 226
let purple              = make_color 160 32 240
let mediumPurple        = make_color 147 112 219
let thistle             = make_color 216 191 216
let snow1               = make_color 255 250 250
let snow2               = make_color 238 233 233
let snow3               = make_color 205 201 201
let snow4               = make_color 139 137 137
let seashell1           = make_color 255 245 238
let seashell2           = make_color 238 229 222
let seashell3           = make_color 205 197 191
let seashell4           = make_color 139 134 130
let antiqueWhite1       = make_color 255 239 219
let antiqueWhite2       = make_color 238 223 204
let antiqueWhite3       = make_color 205 192 176
let antiqueWhite4       = make_color 139 131 120
let bisque1             = make_color 255 228 196
let bisque2             = make_color 238 213 183
let bisque3             = make_color 205 183 158
let bisque4             = make_color 139 125 107
let peachPuff1          = make_color 255 218 185
let peachPuff2          = make_color 238 203 173
let peachPuff3          = make_color 205 175 149
let peachPuff4          = make_color 139 119 101
let navajoWhite1        = make_color 255 222 173
let navajoWhite2        = make_color 238 207 161
let navajoWhite3        = make_color 205 179 139
let navajoWhite4        = make_color 139 121 94
let lemonChiffon1       = make_color 255 250 205
let lemonChiffon2       = make_color 238 233 191
let lemonChiffon3       = make_color 205 201 165
let lemonChiffon4       = make_color 139 137 112
let cornsilk1           = make_color 255 248 220
let cornsilk2           = make_color 238 232 205
let cornsilk3           = make_color 205 200 177
let cornsilk4           = make_color 139 136 120
let ivory1              = make_color 255 255 240
let ivory2              = make_color 238 238 224
let ivory3              = make_color 205 205 193
let ivory4              = make_color 139 139 131
let honeydew1           = make_color 240 255 240
let honeydew2           = make_color 224 238 224
let honeydew3           = make_color 193 205 193
let honeydew4           = make_color 131 139 131
let lavenderBlush1      = make_color 255 240 245
let lavenderBlush2      = make_color 238 224 229
let lavenderBlush3      = make_color 205 193 197
let lavenderBlush4      = make_color 139 131 134
let mistyRose1          = make_color 255 228 225
let mistyRose2          = make_color 238 213 210
let mistyRose3          = make_color 205 183 181
let mistyRose4          = make_color 139 125 123
let azure1              = make_color 240 255 255
let azure2              = make_color 224 238 238
let azure3              = make_color 193 205 205
let azure4              = make_color 131 139 139
let slateBlue1          = make_color 131 111 255
let slateBlue2          = make_color 122 103 238
let slateBlue3          = make_color 105 89 205
let slateBlue4          = make_color 71 60 139
let royalBlue1          = make_color 72 118 255
let royalBlue2          = make_color 67 110 238
let royalBlue3          = make_color 58 95 205
let royalBlue4          = make_color 39 64 139
let blue1               = make_color 0 0 255
let blue2               = make_color 0 0 238
let blue3               = make_color 0 0 205
let blue4               = make_color 0 0 139
let dodgerBlue1         = make_color 30 144 255
let dodgerBlue2         = make_color 28 134 238
let dodgerBlue3         = make_color 24 116 205
let dodgerBlue4         = make_color 16 78 139
let steelBlue1          = make_color 99 184 255
let steelBlue2          = make_color 92 172 238
let steelBlue3          = make_color 79 148 205
let steelBlue4          = make_color 54 100 139
let deepSkyBlue1        = make_color 0 191 255
let deepSkyBlue2        = make_color 0 178 238
let deepSkyBlue3        = make_color 0 154 205
let deepSkyBlue4        = make_color 0 104 139
let skyBlue1            = make_color 135 206 255
let skyBlue2            = make_color 126 192 238
let skyBlue3            = make_color 108 166 205
let skyBlue4            = make_color 74 112 139
let lightSkyBlue1       = make_color 176 226 255
let lightSkyBlue2       = make_color 164 211 238
let lightSkyBlue3       = make_color 141 182 205
let lightSkyBlue4       = make_color 96 123 139
let slateGray1          = make_color 198 226 255
let slateGray2          = make_color 185 211 238
let slateGray3          = make_color 159 182 205
let slateGray4          = make_color 108 123 139
let lightSteelBlue1     = make_color 202 225 255
let lightSteelBlue2     = make_color 188 210 238
let lightSteelBlue3     = make_color 162 181 205
let lightSteelBlue4     = make_color 110 123 139
let lightBlue1          = make_color 191 239 255
let lightBlue2          = make_color 178 223 238
let lightBlue3          = make_color 154 192 205
let lightBlue4          = make_color 104 131 139
let lightCyan1          = make_color 224 255 255
let lightCyan2          = make_color 209 238 238
let lightCyan3          = make_color 180 205 205
let lightCyan4          = make_color 122 139 139
let paleTurquoise1      = make_color 187 255 255
let paleTurquoise2      = make_color 174 238 238
let paleTurquoise3      = make_color 150 205 205
let paleTurquoise4      = make_color 102 139 139
let cadetBlue1          = make_color 152 245 255
let cadetBlue2          = make_color 142 229 238
let cadetBlue3          = make_color 122 197 205
let cadetBlue4          = make_color 83 134 139
let turquoise1          = make_color 0 245 255
let turquoise2          = make_color 0 229 238
let turquoise3          = make_color 0 197 205
let turquoise4          = make_color 0 134 139
let cyan1               = make_color 0 255 255
let cyan2               = make_color 0 238 238
let cyan3               = make_color 0 205 205
let cyan4               = make_color 0 139 139
let darkSlateGray1      = make_color 151 255 255
let darkSlateGray2      = make_color 141 238 238
let darkSlateGray3      = make_color 121 205 205
let darkSlateGray4      = make_color 82 139 139
let aquamarine1         = make_color 127 255 212
let aquamarine2         = make_color 118 238 198
let aquamarine3         = make_color 102 205 170
let aquamarine4         = make_color 69 139 116
let darkSeaGreen1       = make_color 193 255 193
let darkSeaGreen2       = make_color 180 238 180
let darkSeaGreen3       = make_color 155 205 155
let darkSeaGreen4       = make_color 105 139 105
let seaGreen1           = make_color 84 255 159
let seaGreen2           = make_color 78 238 148
let seaGreen3           = make_color 67 205 128
let seaGreen4           = make_color 46 139 87
let paleGreen1          = make_color 154 255 154
let paleGreen2          = make_color 144 238 144
let paleGreen3          = make_color 124 205 124
let paleGreen4          = make_color 84 139 84
let springGreen1        = make_color 0 255 127
let springGreen2        = make_color 0 238 118
let springGreen3        = make_color 0 205 102
let springGreen4        = make_color 0 139 69
let green1              = make_color 0 255 0
let green2              = make_color 0 238 0
let green3              = make_color 0 205 0
let green4              = make_color 0 139 0
let chartreuse1         = make_color 127 255 0
let chartreuse2         = make_color 118 238 0
let chartreuse3         = make_color 102 205 0
let chartreuse4         = make_color 69 139 0
let oliveDrab1          = make_color 192 255 62
let oliveDrab2          = make_color 179 238 58
let oliveDrab3          = make_color 154 205 50
let oliveDrab4          = make_color 105 139 34
let darkOliveGreen1     = make_color 202 255 112
let darkOliveGreen2     = make_color 188 238 104
let darkOliveGreen3     = make_color 162 205 90
let darkOliveGreen4     = make_color 110 139 61
let khaki1              = make_color 255 246 143
let khaki2              = make_color 238 230 133
let khaki3              = make_color 205 198 115
let khaki4              = make_color 139 134 78
let lightGoldenrod1     = make_color 255 236 139
let lightGoldenrod2     = make_color 238 220 130
let lightGoldenrod3     = make_color 205 190 112
let lightGoldenrod4     = make_color 139 129 76
let lightYellow1        = make_color 255 255 224
let lightYellow2        = make_color 238 238 209
let lightYellow3        = make_color 205 205 180
let lightYellow4        = make_color 139 139 122
let yellow1             = make_color 255 255 0
let yellow2             = make_color 238 238 0
let yellow3             = make_color 205 205 0
let yellow4             = make_color 139 139 0
let gold1               = make_color 255 215 0
let gold2               = make_color 238 201 0
let gold3               = make_color 205 173 0
let gold4               = make_color 139 117 0
let goldenrod1          = make_color 255 193 37
let goldenrod2          = make_color 238 180 34
let goldenrod3          = make_color 205 155 29
let goldenrod4          = make_color 139 105 20
let darkGoldenrod1      = make_color 255 185 15
let darkGoldenrod2      = make_color 238 173 14
let darkGoldenrod3      = make_color 205 149 12
let darkGoldenrod4      = make_color 139 101 8
let rosyBrown1          = make_color 255 193 193
let rosyBrown2          = make_color 238 180 180
let rosyBrown3          = make_color 205 155 155
let rosyBrown4          = make_color 139 105 105
let indianRed1          = make_color 255 106 106
let indianRed2          = make_color 238 99 99
let indianRed3          = make_color 205 85 85
let indianRed4          = make_color 139 58 58
let sienna1             = make_color 255 130 71
let sienna2             = make_color 238 121 66
let sienna3             = make_color 205 104 57
let sienna4             = make_color 139 71 38
let burlywood1          = make_color 255 211 155
let burlywood2          = make_color 238 197 145
let burlywood3          = make_color 205 170 125
let burlywood4          = make_color 139 115 85
let wheat1              = make_color 255 231 186
let wheat2              = make_color 238 216 174
let wheat3              = make_color 205 186 150
let wheat4              = make_color 139 126 102
let tan1                = make_color 255 165 79
let tan2                = make_color 238 154 73
let tan3                = make_color 205 133 63
let tan4                = make_color 139 90 43
let chocolate1          = make_color 255 127 36
let chocolate2          = make_color 238 118 33
let chocolate3          = make_color 205 102 29
let chocolate4          = make_color 139 69 19
let firebrick1          = make_color 255 48 48
let firebrick2          = make_color 238 44 44
let firebrick3          = make_color 205 38 38
let firebrick4          = make_color 139 26 26
let brown1              = make_color 255 64 64
let brown2              = make_color 238 59 59
let brown3              = make_color 205 51 51
let brown4              = make_color 139 35 35
let salmon1             = make_color 255 140 105
let salmon2             = make_color 238 130 98
let salmon3             = make_color 205 112 84
let salmon4             = make_color 139 76 57
let lightSalmon1        = make_color 255 160 122
let lightSalmon2        = make_color 238 149 114
let lightSalmon3        = make_color 205 129 98
let lightSalmon4        = make_color 139 87 66
let orange1             = make_color 255 165 0
let orange2             = make_color 238 154 0
let orange3             = make_color 205 133 0
let orange4             = make_color 139 90 0
let darkOrange1         = make_color 255 127 0
let darkOrange2         = make_color 238 118 0
let darkOrange3         = make_color 205 102 0
let darkOrange4         = make_color 139 69 0
let coral1              = make_color 255 114 86
let coral2              = make_color 238 106 80
let coral3              = make_color 205 91 69
let coral4              = make_color 139 62 47
let tomato1             = make_color 255 99 71
let tomato2             = make_color 238 92 66
let tomato3             = make_color 205 79 57
let tomato4             = make_color 139 54 38
let orangeRed1          = make_color 255 69 0
let orangeRed2          = make_color 238 64 0
let orangeRed3          = make_color 205 55 0
let orangeRed4          = make_color 139 37 0
let red1                = make_color 255 0 0
let red2                = make_color 238 0 0
let red3                = make_color 205 0 0
let red4                = make_color 139 0 0
let deepPink1           = make_color 255 20 147
let deepPink2           = make_color 238 18 137
let deepPink3           = make_color 205 16 118
let deepPink4           = make_color 139 10 80
let hotPink1            = make_color 255 110 180
let hotPink2            = make_color 238 106 167
let hotPink3            = make_color 205 96 144
let hotPink4            = make_color 139 58 98
let pink1               = make_color 255 181 197
let pink2               = make_color 238 169 184
let pink3               = make_color 205 145 158
let pink4               = make_color 139 99 108
let lightPink1          = make_color 255 174 185
let lightPink2          = make_color 238 162 173
let lightPink3          = make_color 205 140 149
let lightPink4          = make_color 139 95 101
let paleVioletRed1      = make_color 255 130 171
let paleVioletRed2      = make_color 238 121 159
let paleVioletRed3      = make_color 205 104 137
let paleVioletRed4      = make_color 139 71 93
let maroon1             = make_color 255 52 179
let maroon2             = make_color 238 48 167
let maroon3             = make_color 205 41 144
let maroon4             = make_color 139 28 98
let violetRed1          = make_color 255 62 150
let violetRed2          = make_color 238 58 140
let violetRed3          = make_color 205 50 120
let violetRed4          = make_color 139 34 82
let magenta1            = make_color 255 0 255
let magenta2            = make_color 238 0 238
let magenta3            = make_color 205 0 205
let magenta4            = make_color 139 0 139
let orchid1             = make_color 255 131 250
let orchid2             = make_color 238 122 233
let orchid3             = make_color 205 105 201
let orchid4             = make_color 139 71 137
let plum1               = make_color 255 187 255
let plum2               = make_color 238 174 238
let plum3               = make_color 205 150 205
let plum4               = make_color 139 102 139
let mediumOrchid1       = make_color 224 102 255
let mediumOrchid2       = make_color 209 95 238
let mediumOrchid3       = make_color 180 82 205
let mediumOrchid4       = make_color 122 55 139
let darkOrchid1         = make_color 191 62 255
let darkOrchid2         = make_color 178 58 238
let darkOrchid3         = make_color 154 50 205
let darkOrchid4         = make_color 104 34 139
let purple1             = make_color 155 48 255
let purple2             = make_color 145 44 238
let purple3             = make_color 125 38 205
let purple4             = make_color 85 26 139
let mediumPurple1       = make_color 171 130 255
let mediumPurple2       = make_color 159 121 238
let mediumPurple3       = make_color 137 104 205
let mediumPurple4       = make_color 93 71 139
let thistle1            = make_color 255 225 255
let thistle2            = make_color 238 210 238
let thistle3            = make_color 205 181 205
let thistle4            = make_color 139 123 139
let gray0               = make_color 0 0 0
let gray1               = make_color 3 3 3
let gray2               = make_color 5 5 5
let gray3               = make_color 8 8 8
let gray4               = make_color 10 10 10
let gray5               = make_color 13 13 13
let gray6               = make_color 15 15 15
let gray7               = make_color 18 18 18
let gray8               = make_color 20 20 20
let gray9               = make_color 23 23 23
let gray10              = make_color 26 26 26
let gray11              = make_color 28 28 28
let gray12              = make_color 31 31 31
let gray13              = make_color 33 33 33
let gray14              = make_color 36 36 36
let gray15              = make_color 38 38 38
let gray16              = make_color 41 41 41
let gray17              = make_color 43 43 43
let gray18              = make_color 46 46 46
let gray19              = make_color 48 48 48
let gray20              = make_color 51 51 51
let gray21              = make_color 54 54 54
let gray22              = make_color 56 56 56
let gray23              = make_color 59 59 59
let gray24              = make_color 61 61 61
let gray25              = make_color 64 64 64
let gray26              = make_color 66 66 66
let gray27              = make_color 69 69 69
let gray28              = make_color 71 71 71
let gray29              = make_color 74 74 74
let gray30              = make_color 77 77 77
let gray31              = make_color 79 79 79
let gray32              = make_color 82 82 82
let gray33              = make_color 84 84 84
let gray34              = make_color 87 87 87
let gray35              = make_color 89 89 89
let gray36              = make_color 92 92 92
let gray37              = make_color 94 94 94
let gray38              = make_color 97 97 97
let gray39              = make_color 99 99 99
let gray40              = make_color 102 102 102
let gray41              = make_color 105 105 105
let gray42              = make_color 107 107 107
let gray43              = make_color 110 110 110
let gray44              = make_color 112 112 112
let gray45              = make_color 115 115 115
let gray46              = make_color 117 117 117
let gray47              = make_color 120 120 120
let gray48              = make_color 122 122 122
let gray49              = make_color 125 125 125
let gray50              = make_color 127 127 127
let gray51              = make_color 130 130 130
let gray52              = make_color 133 133 133
let gray53              = make_color 135 135 135
let gray54              = make_color 138 138 138
let gray55              = make_color 140 140 140
let gray56              = make_color 143 143 143
let gray57              = make_color 145 145 145
let gray58              = make_color 148 148 148
let gray59              = make_color 150 150 150
let gray60              = make_color 153 153 153
let gray61              = make_color 156 156 156
let gray62              = make_color 158 158 158
let gray63              = make_color 161 161 161
let gray64              = make_color 163 163 163
let gray65              = make_color 166 166 166
let gray66              = make_color 168 168 168
let gray67              = make_color 171 171 171
let gray68              = make_color 173 173 173
let gray69              = make_color 176 176 176
let gray70              = make_color 179 179 179
let gray71              = make_color 181 181 181
let gray72              = make_color 184 184 184
let gray73              = make_color 186 186 186
let gray74              = make_color 189 189 189
let gray75              = make_color 191 191 191
let gray76              = make_color 194 194 194
let gray77              = make_color 196 196 196
let gray78              = make_color 199 199 199
let gray79              = make_color 201 201 201
let gray80              = make_color 204 204 204
let gray81              = make_color 207 207 207
let gray82              = make_color 209 209 209
let gray83              = make_color 212 212 212
let gray84              = make_color 214 214 214
let gray85              = make_color 217 217 217
let gray86              = make_color 219 219 219
let gray87              = make_color 222 222 222
let gray88              = make_color 224 224 224
let gray89              = make_color 227 227 227
let gray90              = make_color 229 229 229
let gray91              = make_color 232 232 232
let gray92              = make_color 235 235 235
let gray93              = make_color 237 237 237
let gray94              = make_color 240 240 240
let gray95              = make_color 242 242 242
let gray96              = make_color 245 245 245
let gray97              = make_color 247 247 247
let gray98              = make_color 250 250 250
let gray99              = make_color 252 252 252
let gray100             = make_color 255 255 255
let darkGray            = make_color 169 169 169
let darkBlue            = make_color 0 0 139
let darkCyan            = make_color 0 139 139
let darkMagenta         = make_color 139 0 139
let darkRed             = make_color 139 0 0
let lightGreen          = make_color 144 238 144
