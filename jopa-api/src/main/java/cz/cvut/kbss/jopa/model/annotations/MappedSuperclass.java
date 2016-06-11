package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.*;

/**
 * Designates a class whose mapping information is applied to the entities that inherit from it.
 * <p>
 * A mapped superclass itself does not represent any ontological type.
 * A class designated with the MappedSuperclass annotation can be mapped in the same way as an entity except that the mappings
 * will apply only to its subclasses since no OWL class exists for the mapped superclass itself.
 * When applied to the subclasses the inherited mappings will apply in the context of the subclass ontological types.
 */
@Documented
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface MappedSuperclass {
}
