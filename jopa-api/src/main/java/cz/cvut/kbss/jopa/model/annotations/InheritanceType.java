package cz.cvut.kbss.jopa.model.annotations;

/**
 * Defines polymorphic loading strategy options for the {@link Inheritance} annotation.
 * <p>
 * The strategies are described in <a href="https://github.com/kbss-cvut/jopa/issues/1">https://github.com/kbss-cvut/jopa/issues/1</a>.
 */
public enum InheritanceType {

    /**
     * Instances are loading in two steps. First, types are loaded to determine which resulting type should be used,
     * then the actual entity is loaded.
     */
    TWO_STEP,
    /**
     * An attempt to load an entity is made. If the entity is abstract or the corresponding type is missing, actual type
     * is determined from the loaded types and the missing attributes are loaded.
     */
    TRY_FIRST
}
