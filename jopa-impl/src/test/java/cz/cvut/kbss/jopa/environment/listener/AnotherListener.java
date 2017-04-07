package cz.cvut.kbss.jopa.environment.listener;

import cz.cvut.kbss.jopa.model.annotations.PrePersist;

import java.lang.reflect.Method;

public class AnotherListener {

    @PrePersist
    public void prePersist(Object instance) {
        System.out.println("prePersist");
    }

    public static Method getPrePersist() throws NoSuchMethodException {
        return AnotherListener.class.getDeclaredMethod("prePersist", Object.class);
    }
}
