package cz.cvut.kbss.jopa.modelgen.util;

import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.Element;
import java.util.*;

public class TypeUtils {

    private TypeUtils() {
        throw new AssertionError();
    }

    /**
     * Checks whether the specified element contains any of the specified annotations.
     *
     * @param element     Element to examine
     * @param annotations Annotations to look for
     * @return True if any of the annotations are present on the element, false if none is
     */
    public static boolean containsAnnotation(Element element, String... annotations) {
        assert element != null;
        assert annotations != null;

        final Set<String> annotationClassNames = new HashSet<>(Arrays.asList(annotations));

        List<? extends AnnotationMirror> annotationMirrors = element.getAnnotationMirrors();
        for (AnnotationMirror mirror : annotationMirrors) {
            if (annotationClassNames.contains(mirror.getAnnotationType().toString())) {
                return true;
            }
        }
        return false;
    }
}
