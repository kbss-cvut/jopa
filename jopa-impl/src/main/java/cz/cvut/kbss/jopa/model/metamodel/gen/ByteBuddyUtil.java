package cz.cvut.kbss.jopa.model.metamodel.gen;

import net.bytebuddy.NamingStrategy;
import net.bytebuddy.description.type.TypeDescription;

/**
 * ByteBuddy utility functions and classes.
 */
class ByteBuddyUtil {

    private ByteBuddyUtil() {
        throw new AssertionError();
    }

    public static class NameGenerator extends NamingStrategy.AbstractBase {

        @Override
        protected String name(TypeDescription typeDescription) {
            return "JOPA_" + typeDescription.getSimpleName();
        }
    }
}
