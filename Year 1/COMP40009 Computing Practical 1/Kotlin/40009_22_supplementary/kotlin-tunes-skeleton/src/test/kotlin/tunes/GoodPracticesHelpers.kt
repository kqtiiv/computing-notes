package tunes

import kotlin.reflect.KClass
import kotlin.reflect.KVisibility
import kotlin.test.assertTrue

fun checkAllOtherMembersFinal(
    targetClass: KClass<*>,
    vararg expectedNonFinalMembers: String,
) {
    val allowedNames: Set<String> = expectedNonFinalMembers.toSet()
    var ok = true
    for (member in targetClass.members) {
        if (!member.isFinal) {
            if (allowedNames.contains(member.name)) {
                continue
            }
            System.err.println("Non-final member: ${member.name} in class ${targetClass.simpleName}")
            ok = false
        }
    }
    assertTrue(ok)
}

fun checkAllOtherMembersPrivate(
    targetClass: KClass<*>,
    vararg expectedPublicMembers: String,
) {
    val allowedNames: Set<String> = expectedPublicMembers.toSet()
    var ok = true
    for (member in targetClass.members) {
        if (member.visibility!! == KVisibility.PRIVATE) {
            continue
        }
        if (allowedNames.contains(member.name)) {
            continue
        }
        System.err.println(
            "Unexpected non-private member ${member.name} in class ${targetClass.simpleName}",
        )
        ok = false
    }
    assertTrue(ok)
}
