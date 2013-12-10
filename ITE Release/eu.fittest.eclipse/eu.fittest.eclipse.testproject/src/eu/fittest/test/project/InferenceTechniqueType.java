/**
 */
package eu.fittest.test.project;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.Enumerator;

/**
 * <!-- begin-user-doc -->
 * A representation of the literals of the enumeration '<em><b>Inference Technique Type</b></em>',
 * and utility methods for working with them.
 * <!-- end-user-doc -->
 * @see eu.fittest.test.project.ProjectPackage#getInferenceTechniqueType()
 * @model extendedMetaData="name='InferenceTechniqueType'"
 * @generated
 */
public enum InferenceTechniqueType implements Enumerator {
	/**
	 * The '<em><b>SEQUENCEBASED</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #SEQUENCEBASED_VALUE
	 * @generated
	 * @ordered
	 */
	SEQUENCEBASED(0, "SEQUENCEBASED", "SEQUENCE-BASED"),

	/**
	 * The '<em><b>STATEBASED</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #STATEBASED_VALUE
	 * @generated
	 * @ordered
	 */
	STATEBASED(1, "STATEBASED", "STATE-BASED"),

	/**
	 * The '<em><b>AUTOABS</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #AUTOABS_VALUE
	 * @generated
	 * @ordered
	 */
	AUTOABS(2, "AUTOABS", "AUTO-ABS");

	/**
	 * The '<em><b>SEQUENCEBASED</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>SEQUENCEBASED</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #SEQUENCEBASED
	 * @model literal="SEQUENCE-BASED"
	 * @generated
	 * @ordered
	 */
	public static final int SEQUENCEBASED_VALUE = 0;

	/**
	 * The '<em><b>STATEBASED</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>STATEBASED</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #STATEBASED
	 * @model literal="STATE-BASED"
	 * @generated
	 * @ordered
	 */
	public static final int STATEBASED_VALUE = 1;

	/**
	 * The '<em><b>AUTOABS</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>AUTOABS</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #AUTOABS
	 * @model literal="AUTO-ABS"
	 * @generated
	 * @ordered
	 */
	public static final int AUTOABS_VALUE = 2;

	/**
	 * An array of all the '<em><b>Inference Technique Type</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final InferenceTechniqueType[] VALUES_ARRAY =
		new InferenceTechniqueType[] {
			SEQUENCEBASED,
			STATEBASED,
			AUTOABS,
		};

	/**
	 * A public read-only list of all the '<em><b>Inference Technique Type</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static final List<InferenceTechniqueType> VALUES = Collections.unmodifiableList(Arrays.asList(VALUES_ARRAY));

	/**
	 * Returns the '<em><b>Inference Technique Type</b></em>' literal with the specified literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static InferenceTechniqueType get(String literal) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			InferenceTechniqueType result = VALUES_ARRAY[i];
			if (result.toString().equals(literal)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>Inference Technique Type</b></em>' literal with the specified name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static InferenceTechniqueType getByName(String name) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			InferenceTechniqueType result = VALUES_ARRAY[i];
			if (result.getName().equals(name)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>Inference Technique Type</b></em>' literal with the specified integer value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static InferenceTechniqueType get(int value) {
		switch (value) {
			case SEQUENCEBASED_VALUE: return SEQUENCEBASED;
			case STATEBASED_VALUE: return STATEBASED;
			case AUTOABS_VALUE: return AUTOABS;
		}
		return null;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private final int value;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private final String name;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private final String literal;

	/**
	 * Only this class can construct instances.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private InferenceTechniqueType(int value, String name, String literal) {
		this.value = value;
		this.name = name;
		this.literal = literal;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getValue() {
	  return value;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getName() {
	  return name;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getLiteral() {
	  return literal;
	}

	/**
	 * Returns the literal value of the enumerator, which is its string representation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String toString() {
		return literal;
	}
	
} //InferenceTechniqueType
