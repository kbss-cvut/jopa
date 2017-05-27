package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.*;

/**
 * Wrapper for declaration of multiple namespaces on an element.
 * <p>
 * Usage example:
 * <pre>
 * <code>
 * {@literal @}Namespaces({{@literal @}Namespace("xs", "http://www.w3.org/2001/XMLSchema#"),
 *                         {@literal @}Namespace("owl", "http://www.w3.org/2002/07/owl#")})
 * </code>
 * </pre>
 *
 * @see Namespace
 */
@Inherited
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.PACKAGE, ElementType.TYPE})
public @interface Namespaces {

    Namespace[] value();
}
