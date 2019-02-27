/**
 * Copyright (C) 2019 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package com.sun.codemodel;

import java.lang.annotation.Annotation;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Represents an annotation on a program element.
 * <p>
 * FIX to com.sun.codemodel.JAnnotationUse supporting JFieldRef as annotation values (constants)
 */
public final class JAnnotationUse extends JAnnotationValue {

    /**
     * The {@link Annotation} class
     */
    private final JClass clazz;

    /**
     * Map of member values.
     */
    private Map<String, JAnnotationValue> memberValues;

    JAnnotationUse(JClass clazz) {
        this.clazz = clazz;
    }

    private JCodeModel owner() {
        return clazz.owner();
    }

    private void addValue(String name, JAnnotationValue annotationValue) {
        // Use ordered map to keep the code generation the same on any JVM.
        // Lazily created.
        if (memberValues == null)
            memberValues = new LinkedHashMap<>();
        memberValues.put(name, annotationValue);
    }

    /**
     * Adds a member value pair to this annotation
     *
     * @param name  The simple name for this annotation
     * @param value The boolean value for this annotation
     * @return The JAnnotationUse. More member value pairs can be added to it using the same or the overloaded methods.
     */
    public JAnnotationUse param(String name, boolean value) {
        addValue(name, new JAnnotationStringValue(JExpr.lit(value)));
        return this;
    }

    /**
     * Adds a member value pair to this annotation
     *
     * @param name  The simple name for this annotation
     * @param value The byte member value for this annotation
     * @return The JAnnotationUse. More member value pairs can be added to it using the same or the overloaded methods.
     */
    public JAnnotationUse param(String name, byte value) {
        addValue(name, new JAnnotationStringValue(JExpr.lit(value)));
        return this;
    }

    /**
     * Adds a member value pair to this annotation
     *
     * @param name  The simple name for this annotation
     * @param value The char member value for this annotation
     * @return The JAnnotationUse. More member value pairs can be added to it using the same or the overloaded methods.
     */
    public JAnnotationUse param(String name, char value) {
        addValue(name, new JAnnotationStringValue(JExpr.lit(value)));
        return this;
    }

    /**
     * Adds a member value pair to this annotation
     *
     * @param name  The simple name for this annotation
     * @param value The double member value for this annotation
     * @return The JAnnotationUse. More member value pairs can be added to it using the same or the overloaded methods.
     */
    public JAnnotationUse param(String name, double value) {
        addValue(name, new JAnnotationStringValue(JExpr.lit(value)));
        return this;
    }

    /**
     * Adds a member value pair to this annotation
     *
     * @param name  The simple name for this annotation
     * @param value The float member value for this annotation
     * @return The JAnnotationUse. More member value pairs can be added to it using the same or the overloaded methods.
     */
    public JAnnotationUse param(String name, float value) {
        addValue(name, new JAnnotationStringValue(JExpr.lit(value)));
        return this;
    }

    /**
     * Adds a member value pair to this annotation
     *
     * @param name  The simple name for this annotation
     * @param value The long member value for this annotation
     * @return The JAnnotationUse. More member value pairs can be added to it using the same or the overloaded methods.
     */
    public JAnnotationUse param(String name, long value) {
        addValue(name, new JAnnotationStringValue(JExpr.lit(value)));
        return this;
    }

    /**
     * Adds a member value pair to this annotation
     *
     * @param name  The simple name for this annotation
     * @param value The short member value for this annotation
     * @return The JAnnotationUse. More member value pairs can be added to it using the same or the overloaded methods.
     */
    public JAnnotationUse param(String name, short value) {
        addValue(name, new JAnnotationStringValue(JExpr.lit(value)));
        return this;
    }

    /**
     * Adds a member value pair to this annotation
     *
     * @param name  The simple name for this annotation
     * @param value The int member value for this annotation
     * @return The JAnnotationUse. More member value pairs can be added to it using the same or the overloaded methods.
     */
    public JAnnotationUse param(String name, int value) {
        addValue(name, new JAnnotationStringValue(JExpr.lit(value)));
        return this;
    }

    /**
     * Adds a member value pair to this annotation
     *
     * @param name  The simple name for this annotation
     * @param value The String member value for this annotation
     * @return The JAnnotationUse. More member value pairs can be added to it using the same or the overloaded methods.
     */
    public JAnnotationUse param(String name, String value) {
        //Escape string values with quotes so that they can
        //be generated accordingly
        addValue(name, new JAnnotationStringValue(JExpr.lit(value)));
        return this;
    }

    /**
     * Adds a member value pair to this annotation
     * For adding class values as param
     *
     * @param name  The simple name for this annotation
     * @param value The annotation class which is member value for this annotation
     * @return The JAnnotationUse. More member value pairs can be added to it using the same or the overloaded methods.
     * @see #param(String, Class)
     */
    public JAnnotationUse annotationParam(String name, Class<? extends Annotation> value) {
        JAnnotationUse annotationUse = new JAnnotationUse(owner().ref(value));
        addValue(name, annotationUse);
        return annotationUse;
    }

    /**
     * Adds a member value pair to this annotation
     *
     * @param name  The simple name for this annotation
     * @param value The enum class which is member value for this annotation
     * @return The JAnnotationUse. More member value pairs can be added to it using the same or the overloaded methods.
     */
    public JAnnotationUse param(String name, final Enum<?> value) {
        addValue(name, new JAnnotationValue() {
            public void generate(JFormatter f) {
                f.t(owner().ref(value.getDeclaringClass())).p('.').p(value.name());
            }
        });
        return this;
    }

    /**
     * Adds a member value pair to this annotation
     *
     * @param name  The simple name for this annotation
     * @param value The JEnumConstant which is member value for this annotation
     * @return The JAnnotationUse. More member value pairs can be added to it using the same or the overloaded methods.
     */
    public JAnnotationUse param(String name, JEnumConstant value) {
        addValue(name, new JAnnotationStringValue(value));
        return this;
    }

    /**
     * Adds a member value pair to this annotation
     * <p>
     * This can be used for e.g to specify
     * <pre>
     *        &#64;XmlCollectionItem(type=Integer.class);
     * </pre>
     * For adding a value of {@code Class<? extends Annotation>}
     *
     * @param name  The simple name for this annotation param
     * @param value The class type of the param
     * @return The JAnnotationUse. More member value pairs can be added to it using the same or the overloaded methods.
     * @see #annotationParam(String, Class)
     */
    public JAnnotationUse param(String name, final Class<?> value) {
        addValue(name, new JAnnotationStringValue(
                new JExpressionImpl() {
                    @Override
                    public void generate(JFormatter f) {
                        f.p(value.getName().replace('$', '.'));
                        f.p(".class");
                    }
                }));
        return this;
    }

    /**
     * Adds a member value pair to this annotation based on the
     * type represented by the given JType
     *
     * @param name The simple name for this annotation param
     * @param type the JType representing the actual type
     * @return The JAnnotationUse. More member value pairs can be added to it using the same or the overloaded methods.
     */
    public JAnnotationUse param(String name, JType type) {
        JClass cls = type.boxify();
        addValue(name, new JAnnotationStringValue(cls.dotclass()));
        return this;
    }

    /**
     * Adds a member value pair which is of type array to this annotation
     *
     * @param name The simple name for this annotation
     * @return The JAnnotationArrayMember. For adding array values
     * @see JAnnotationArrayMember
     */
    public JAnnotationArrayMember paramArray(String name) {
        JAnnotationArrayMember arrayMember = new JAnnotationArrayMember(owner());
        addValue(name, arrayMember);
        return arrayMember;
    }


//    /**
//     * This can be used to add annotations inside annotations
//     * for e.g  &#64;XmlCollection(values= &#64;XmlCollectionItem(type=Foo.class))
//     * @param className
//     *         The classname of the annotation to be included
//     * @return
//     *         The JAnnotationUse that can be used as a member within this JAnnotationUse
//     * @deprecated
//     *      use {@link JAnnotationArrayMember#annotate}
//     */
//    public JAnnotationUse annotate(String className) {
//        JAnnotationUse annotationUse = new JAnnotationUse(owner().ref(className));
//        return annotationUse;
//    }

    /**
     * This can be used to add annotations inside annotations
     * for e.g  &#64;XmlCollection(values= &#64;XmlCollectionItem(type=Foo.class))
     *
     * @param clazz The annotation class to be included
     * @return The JAnnotationUse that can be used as a member within this JAnnotationUse
     * @deprecated use {@link JAnnotationArrayMember#annotate}
     */
    public JAnnotationUse annotate(Class<? extends Annotation> clazz) {
        return new JAnnotationUse(owner().ref(clazz));
    }

    @Override
    public void generate(JFormatter f) {
        f.p('@').g(clazz);
        if (memberValues != null) {
            f.p('(');
            boolean first = true;

            if (isOptimizable()) {
                // short form
                f.g(memberValues.get("value"));
            } else {
                for (Map.Entry<String, JAnnotationValue> mapEntry : memberValues.entrySet()) {
                    if (!first) {
                        f.p(',');
                    }
                    f.p(mapEntry.getKey()).p('=').g(mapEntry.getValue());
                    first = false;
                }
            }
            f.p(')');
        }
    }

    private boolean isOptimizable() {
        return memberValues.size() == 1 && memberValues.containsKey("value");
    }

    public JAnnotationUse param(String name, JFieldRef value) {
        addValue(name, new JAnnotationStringValue(value));
        return this;
    }
}
