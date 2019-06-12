package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.*;


/**
 * Marks an attribute whose value is a lexical form of a literal value.
 * <p>
 * This annotation should be used on {@code String} attributes, as literal lexical form is always a string. Lexical form
 * of a literal of any datatype can be loaded. However, saving the lexical form will result in a simple literal, as any
 * other datatype cannot be determined.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface LexicalForm {
}
