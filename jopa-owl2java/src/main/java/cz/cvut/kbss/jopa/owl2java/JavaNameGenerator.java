package cz.cvut.kbss.jopa.owl2java;

import org.semanticweb.owlapi.model.IRI;

import java.text.Normalizer;
import java.util.Arrays;

/**
 * Generates Java names based on IRI identifiers.
 */
public class JavaNameGenerator {

    private static final String[] JAVA_KEYWORDS = {"abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue", "default", "do", "double", "else", "enum", "extends", "final", "finally", "float", "for", "goto", "if", "implements", "import", "instanceof", "int", "interface", "long", "native", "new", "package", "private", "protected", "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this", "throw", "throws", "transient", "try", "void", "volatile", "while"};

    /**
     * Returns a valid Java identifier extracted from the specified IRI.
     * <p>
     * If the IRI contains a non-empty fragment, it is used. Otherwise, the part after the last slash is used as the
     * name.
     *
     * @param iri IRI to extract name from
     * @return Java name based on the specified IRI
     */
    public String generateJavaNameForIri(IRI iri) {
        if (iri.getFragment() != null && !iri.getFragment().isEmpty()) {
            return makeNameValidJava(iri.getFragment());
        } else {
            String strIri = iri.toString();
            if (strIri.charAt(strIri.length() - 1) == '/') {
                strIri = strIri.substring(0, strIri.length() - 1);
            }
            int x = strIri.lastIndexOf("/");
            return makeNameValidJava(strIri.substring(x + 1));
        }
    }

    /**
     * Returns the specified name sanitized for Java.
     * <p>
     * This means the result of this function can be used as/in a Java variable/field/class name.
     *
     * @param name The name to sanitize
     * @return Valid Java identifier
     */
    public static String makeNameValidJava(String name) {
        String res = name.trim().replace("-", "_").replace("'", "_quote_")
                         .replace(".", "_dot_").replace(',', '_')
                         .replace("#", "");
        // Replace non-ASCII characters with ASCII ones
        res = Normalizer.normalize(res, Normalizer.Form.NFD).replaceAll("[^\\p{ASCII}]", "");
        if (Arrays.binarySearch(JAVA_KEYWORDS, res) >= 0) {
            res = "_" + res;
        }
        return res;
    }

    /**
     * Converts the specified name to the Java camel case notation.
     * <p>
     * This process removes underscores used to generate the name.
     *
     * @param name Generated name
     * @return Converted camel case name
     */
    public static String toCamelCaseNotation(String name) {
        StringBuilder result = new StringBuilder();
        for (String w : name.split("_")) {
            if (!w.isEmpty()) {
                result.append(w.substring(0, 1).toUpperCase()).append(w.substring(1));
            }
        }
        return result.toString();
    }
}
