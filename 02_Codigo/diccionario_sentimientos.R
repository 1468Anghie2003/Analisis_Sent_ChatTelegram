# =====================================================================
#  DICCIONARIO DE LOS 15 SENTIMIENTOS (lo carga F06)
#  Estructura acordada en clase: S(polaridad, definición, términos, raíces)
#   - polaridad: 1 positivo, 0 neutro, -1 negativo
#   - términos: palabras o frases exactas (se comparan sin tildes)
#   - raíces: inicio de palabra ("enoj" -> enojado, enojada, enojo...)
#  BASE_CLASE  : términos acordados en clase (se respetan tal cual)
#  AMPLIACION  : términos agregados al explorar el dataset (F02-F05),
#                con jerga juvenil, boliviana y paceña del chat
#  Cambio acordado: amor -> frustración; vergüenza -> satisfacción.
#  Los términos de "amor" y "vergüenza" no se pierden: se reasignan.
# =====================================================================
S <- function(polaridad, definicion, terminos, raices = character())
  list(polaridad = polaridad, definicion = definicion, terminos = terminos, raices = raices)

BASE_CLASE <- list(
  alegria = S(1, "Sentimiento grato y vivo que suele manifestarse con signos exteriores.",
    c("alegre","alegria","feliz","felices","felicidad","contento","contenta","jaja","jeje","xd","genial","buenisimo","buenisima","bacan","chevere","divertido","divertida","festejar","celebrar"),
    c("jaja","jeje","aleg")),
  gratitud = S(1, "Sentimiento que nos obliga a estimar el beneficio recibido.",
    c("gracias","grax","agradezco","agradecido","agradecida","mil gracias","muchas gracias","bendiciones","dios le pague"), c("agradec")),
  esperanza = S(1, "Estado de ánimo en el que se nos presenta como posible lo que deseamos.",
    c("ojala","espero","esperemos","esperanza","confio","confiamos","suerte","exito","exitos","animo","animos","lograremos","podemos lograrlo","vamos a poder"), c("esperanz")),
  frustracion = S(-1, "Imposibilidad de satisfacer una necesidad o un deseo; sensación de chocar contra un obstáculo.",
    c("frustrado","frustrada","frustracion","frustrante"), c("frustr")),
  admiracion = S(1, "Sentimiento de aprecio hacia alguien por sus cualidades o méritos.",
    c("admiro","admiracion","excelente","crack","capo","genio","increible","bravo","felicitaciones","felicidades","respeto","recomendado","recomendada","recomiendo","recomendable"), c("admir","felicit")),
  tranquilidad = S(1, "Cualidad de quien está sosegado y en paz.",
    c("tranquilo","tranquila","tranquilidad","calma","calmado","relajado","paz","descuida","tranqui","sin problema","no hay problema","aliviado","alivio","menos mal"), c("tranqui","relaj","relax")),
  sorpresa = S(0, "Sentimiento que se produce por algo imprevisto, raro o incomprensible.",
    c("wow","guau","sorpresa","sorprendido","sorprendida","imposible","uf","ufff","dios mio","no puede ser","no lo puedo creer","de repente","inesperado","insolito","raro","rara"), c("sorpren","inesper")),
  confusion = S(-1, "Falta de claridad, desorden o perplejidad.",
    c("confundido","confundida","confusion","confuso","confusa","no entiendo","no entendi","nose","ni idea","duda","dudas","dudo","perdido","perdida","me perdi","como asi"), c("confu")),
  preocupacion = S(-1, "Estado de ánimo de quien está intranquilo o inquieto por algo.",
    c("preocupado","preocupada","preocupacion","inquieto","nervioso","nerviosa","nervios","ansiedad","ansioso","ansiosa","miedo","temo","urgente","apurate","apurense","riesgo","peligro","angustia"), c("preocup","nervi","angust")),
  tristeza = S(-1, "Aflicción o pesadumbre.",
    c("triste","tristeza","lamento","lamentable","que pena","penita","lastima","llorar","lloro","duele","sufrir","sufro","deprimido","deprimida","desanimado","descontento"), c("trist","llor","lament","deprim")),
  enojo = S(-1, "Movimiento del ánimo que suscita ira contra alguien.",
    c("enojado","enojada","enojo","molesto","molesta","bronca","rabia","furioso","furiosa","harto","harta","ira","fastidio","odio","pesimo","pesima","horrible","asco","maldito","maldita","mierda","carajo","chucha","puta","pucha","joder","estupido","idiota","inutil","basura","mal","malo","mala","peor","reclamo","queja","injusto","abuso","envidia"), c("enoj","moles","pesim","reclam","injust")),
  decepcion = S(-1, "Pesar causado por un desengaño.",
    c("decepcion","decepcionado","decepcionada","decepciono","desilusion","desilusionado","fraude","engano","mentira","mintieron","falso","falsa","no sirve","para nada","fracaso","fracase","reprobe","reprobado","reprobaron","aplazado","jale","jalaron"), c("decep","desilus")),
  cansancio = S(-1, "Falta de fuerzas que resulta de haberse fatigado.",
    c("cansado","cansada","cansancio","agotado","agotada","agotador","fatiga","estres","estresado","estresada","estresante","pesado","pesada","tedioso","aburrido","aburrida","aburre","aburrimiento","flojera"), c("cansa","agota","estres","aburr")),
  satisfaccion = S(1, "Placer o gusto que se siente al cumplirse un deseo o resolverse un problema.",
    c("satisfecho","satisfecha","satisfaccion","conforme","logrado"), c("satisf")),
  curiosidad = S(0, "Deseo de saber o averiguar alguna cosa.",
    c("curioso","curiosa","curiosidad","interesante","me interesa","quisiera saber","quiero saber","me gustaria saber","cuentame mas"), c("curios"))
)

# Términos de clase de los sentimientos reemplazados (no se pierden)
REASIGNADOS <- list(
  admiracion = c("amor","amo","te amo","te quiero","carino","corazon","hermoso","hermosa","bonito","bonita","lindo","linda","abrazo","abrazos"),
  decepcion  = c("pena ajena","avergonzado","avergonzada","ridiculo","ridicula","papelon","humillante","humillacion","bochorno","penoso","que oso","mi culpa","cague"),
  enojo      = c("verguenza","que verguenza")
)

# Ampliación a partir del dataset (frases de 2+ palabras se buscan completas)
AMPLIACION <- list(
  alegria = c("jajaja","jsjs","haha","que bien","que bueno","me alegro","que alegria","excelente noticia","buena noticia","buenas noticias","divertidisimo","me encanta","encanta","lol","bacansisimo","chido","chevere","de lujo","que lindo"),
  gratitud = c("gracias por","se agradece","se le agradece","se les agradece","muy amable","dios te pague","dios les pague","le agradeceria","les agradeceria","te agradeceria","thx","thanks","graciass","gracia","muchisimas gracias","gracias bro","gracias amigo","gracias amiga","gracias companero","te pasaste","que amable","mil gracias"),
  esperanza = c("esperamos que","crucemos los dedos","con fe","dios quiera","si dios quiere","a ver si","falta poco","vamos que se puede","animense","animate","fuerza","tu puedes","no te rindas","no pierdan la esperanza","ojala se habilite","ojala habiliten","ojala abran","ya casi","estamos cerca","que se abra","esperanzado"),
  frustracion = c("sin cupo","sin cupos","no hay cupo","no hay cupos","no me deja","no me sale","no me salen","no me aparece","no me aparecen","no puedo inscribirme","no puedo ingresar","no puedo pagar","no puedo entrar","no se puede","se cayo","se cae","colapso","colapsado","colapsada","otra vez","de nuevo lo mismo","siempre lo mismo","siempre pasa","no funciona","no carga","error","que mala suerte","mala suerte","chuta","chale","chales","ni modo","nimodo","ya fue","toca esperar","hasta cuando van","hasta cuando vamos","tanto tiempo","burocracia","tramitologia","me cerraron","se cerro","se cerraron","no alcance","no alcanzo","perdi el cupo","nadie responde","nadie contesta","no responden","no contestan","en vano"),
  admiracion = c("ensena bien","ensena muy bien","explica bien","explica muy bien","el mejor","la mejor","los mejores","buen docente","buena docente","buen lic","buena lic","buen licenciado","buena licenciada","buen profe","buena profe","muy bueno","muy buena","se aprende","aprendes mucho","domina","sabe mucho","maestro","maestra","muy recomendable","lo recomiendo","la recomiendo","te lo recomiendo","accesible","tolerante","paciente","flexible","organizado","organizada","respetuoso","claro al explicar","excelente docente","un capo","una capa","top"),
  tranquilidad = c("no te preocupes","no se preocupen","tranqui","con calma","todo bien","esta bien","no pasa nada","confirmado","confirmada","sin problemas","facil","facilito","relax","fresco","sin presion","no hay apuro","todavia hay tiempo","dale","ok","vale","entendido"),
  sorpresa = c("enserio","en serio","wow","guau","no sabia","no puede ser","increible","de la nada","recien me entero","asu","uy","nmms","no manches","que sorpresa","impresionante","de verdad"),
  confusion = c("no entiendo","no entendi","no se si","no se como","no se donde","no se que","no estoy seguro","no estoy segura","confundido","confundida","sera que","alguien me explica","como funciona","que significa","no me queda claro","no queda claro","no tengo idea","ni idea","que paso","por que me sale","porque me sale","estoy perdido","estoy perdida","no comprendo","extrano"),
  preocupacion = c("urgente","ayuda","ayuda porfa","ayuda por favor","que hago","no se que hacer","alguien me ayude","alguien me puede ayudar","auxilio","me asuste","me preocupa","da miedo","me da miedo","tengo miedo","panico","nervios","plazo","ultimo dia","hoy es el ultimo","se acaba el plazo","apurense","rapido","se va a cerrar","van a cerrar","perder la materia","perder el deposito","se pierde","riesgo","cuidado","ojo","alerta"),
  tristeza = c("que triste","que pena","me duele","llorar","lloro","lloramos","me hace llorar","penoso","lastima","rip","sad","triste realidad","se me fue","perdi la materia","no aprobe","me fue mal"),
  enojo = c("injusticia","injusto","abuso","verguenza","indignante","indignado","una burla","falta de respeto","es el colmo","el colmo","pesima atencion","no sirve","inutiles","precario","precarias","nos roban","robo","que les pasa","hasta la madre","alv","rabia","bronca","hartos","queja","quejas","reclamar","exigir","exijamos","deberian","no respetan","irresponsable","irresponsables","mediocre","mediocres","descarado","sinverguenza","atropello"),
  decepcion = c("pense que","crei que","esperaba mas","de nada sirvio","no vale la pena","mala experiencia","me tiro","me jalo","nos jalo","nos tiro","jalaron","jale","aplazado","aplazaron","reprobe","reprobaron","reprobado","gg","no cumplen","no cumplieron","prometieron","otra decepcion","que decepcion","lamentablemente","tristemente","al final no","no abrieron","no se abrio","no habilitaron","se cancelo","cancelaron"),
  cansancio = c("cansado","cansada","agotado","agotada","flojera","sueno","desvelado","desvelada","ya no doy","me rindo","ya no puedo","tedioso","pesado","pesada","estresante","estres","mucha tarea","muchas tareas","demasiado","exigente","dificil","muy dificil","complicado","complicada","rudo","ruda","matado","matada","quemado"),
  satisfaccion = c("ya me inscribi","ya me pude inscribir","lo logre","logre","por fin","al fin","ya pude","aprobe","aprobe con","pase con","pase la materia","me fue bien","me funciono","funciono","lo consegui","listo","perfecto","resuelto","solucionado","se soluciono","ya se arreglo","ya esta habilitado","se habilito","se abrio","abrieron","habilitaron","vale la pena","valio la pena","bien hecho","contento con"),
  curiosidad = c("alguien sabe","alguien sabes","saben si","sabe alguien","sabes si","una consulta","consulta","una pregunta","pregunta","me pueden decir","me podrian decir","alguien conoce","alguien tiene","que tal es","que tal el","que tal la","como es el lic","como es la lic","como es el docente","quien recomiendan","que recomiendan","recomiendan","informacion","me interesa","interesante","quisiera saber","quiero saber","alguien paso","alguien tomo","alguien que haya","como fue","que se sabe")
)

# Señales de ironía: con ellas, los términos positivos del mensaje se
# reinterpretan como decepción (ej. "excelente atención de kardex como siempre 🙃")
IRONIA <- c("como siempre","claro que si","si claro","que organizados","que organizado","tan organizados","que eficiente","excelente atencion",
            "gracias por nada","felicidades kardex","bravo kardex","muy bonito","que bonito","lo de siempre","nada nuevo","genial otra vez")
IRONIA_EMOJI <- c("🙃", "🤡", "🙄")

# Negadores e intensificadores
NEGADORES <- c("no", "nunca", "ni", "tampoco", "jamas", "sin", "nada")
INTENSIFICADORES <- c("muy", "re", "full", "super", "harto", "bien", "demasiado", "tan", "bastante", "mega", "recontra", "sumamente", "extremadamente")
