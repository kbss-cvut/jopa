package cz.cvut.kbss.jopa.query.parameter;

/**
 * Utility class for converting SOQL {@code LIKE} pattern strings to equivalent SPARQL Regex patterns. The conversion
 * follows the typical semantics of {@code LIKE}:
 *
 * <ul>
 *   <li>{@code %} is translated to {@code .*}, matching any sequence of characters (including the empty
 *       sequence).</li>
 *   <li>{@code _} is translated to {@code .}, matching exactly one character.</li>
 *   <li>All other characters are treated as literals. Characters that have special meaning in regular
 *       expressions are escaped so that they are interpreted literally.</li>
 *   <li>Escape sequences introduced with a backslash ({@code \}) cause the following character to be
 *       treated as a literal, regardless of whether it would otherwise be a wildcard.</li>
 * </ul>
 */
public class LikePatternTranslator {

    private LikePatternTranslator() {
        throw new AssertionError();
    }

    /**
     * Translates the input string to a regex pattern.
     *
     * @param input Input to translate
     * @return Resulting pattern
     */
    public static String translate(String input) {
        assert input != null;
        final StringBuilder sb = new StringBuilder(input.length());
        for (int i = 0; i < input.length(); i++) {
            final char c = input.charAt(i);
            if (c == '\\' && i + 1 < input.length()) {
                // Escaped character - append it as a regex literal
                appendEscaped(sb, input.charAt(++i));
            } else if (c == '%') {
                sb.append(".*");
            } else if (c == '_') {
                sb.append('.');
            } else {
                appendEscaped(sb, c);
            }
        }
        return sb.toString();
    }

    private static void appendEscaped(StringBuilder sb, char c) {
        if ("\\^$.|?*+()[]{}".indexOf(c) >= 0) {
            sb.append('\\');
        }
        sb.append(c);
    }
}
