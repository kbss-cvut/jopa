package cz.cvut.kbss.jopa.model.annotations;

/**
 * Defines mapping for enumerated types.
 * <p>
 * The constants of this enumerated type specify how a persistent property or field of an enumerated type should be
 * persisted.
 */
public enum EnumType {
    /**
     * Persist enumerated type property or field as an individual assumed to be an element of a {@literal owl:oneOf}
     * enumeration.
     * <p>
     * Note that in this case the enum constants must be annotated with {@link Individual} mapping them to ontological
     * individuals.
     */
    ONE_OF,

    /**
     * Persist enumerated type property or field as an integer representing the ordinal number of the enumerated
     * constant.
     */
    ORDINAL,

    /**
     * Persist enumerated type property or field as a string representation of the enumerated constant.
     */
    STRING
}
