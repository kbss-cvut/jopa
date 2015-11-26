package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.*;

/**
 * Specifies that the property or field is not persistent. It is used to annotate a property or field of an entity
 * class, mapped superclass, or embeddable class.
 * <p>
 * Example:
 * <pre>
 * <code>{@literal @}Entity
 *  public class Employee {
 *      {@literal @}Id int id;
 *      {@literal @}Transient User currentUser;
 *       ...
 * }
 * </code>
 * </pre>
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Transient {
}
