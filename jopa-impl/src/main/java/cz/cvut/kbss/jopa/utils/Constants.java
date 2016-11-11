package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.model.annotations.InheritanceType;

/**
 * Declares some constants and default values used in JOPA.
 */
public class Constants {

    /**
     * Default inheritance type, used by the {@link cz.cvut.kbss.jopa.model.annotations.Inheritance} annotation.
     */
    public static final InheritanceType DEFAULT_INHERITANCE_TYPE = InheritanceType.TWO_STEP;
}
