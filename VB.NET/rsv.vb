' (C) Stefan John / Stenway / Stenway.com / 2023 

Imports System
Imports System.Collections.Generic
Imports System.Linq
Imports System.Text
Imports System.IO

Namespace Rsv
	Class Program
		Private Shared Function EncodeRsv(rows As String()()) As Byte()
			Dim parts As New List(Of Byte())()
			Dim valueTerminatorByte As Byte() = New Byte() {&Hff}
			Dim nullValueByte As Byte() = New Byte() {&Hfe}
			Dim rowTerminatorByte As Byte() = New Byte() {&Hfd}
			Dim encoder As New UTF8Encoding(False, True)
			For Each row As String() In rows
				For Each value As String In row
					If value Is Nothing Then
						parts.Add(nullValueByte)
					ElseIf value.Length > 0 Then
						parts.Add(encoder.GetBytes(value))
					End If
					parts.Add(valueTerminatorByte)
				Next
				parts.Add(rowTerminatorByte)
			Next
			Dim result As Byte() = New Byte(parts.Sum(Function(part) part.Length) - 1) {}
			Dim stream As New MemoryStream(result)
			For Each part As Byte() In parts
				stream.Write(part, 0, part.Length)
			Next
			Return result
		End Function

		Private Shared Function DecodeRsv(bytes As Byte()) As String()()
			If bytes.Length > 0 AndAlso bytes(bytes.Length - 1) <> &Hfd Then
				Throw New Exception("Incomplete RSV document")
			End If
			Dim decoder As New UTF8Encoding(False, True)
			Dim result As New List(Of String())()
			Dim currentRow As New List(Of String)()
			Dim valueStartIndex As Integer = 0
			For i As Integer = 0 To bytes.Length - 1
				If bytes(i) = &Hff Then
					Dim length As Integer = i - valueStartIndex
					If length = 0 Then
						currentRow.Add("")
					ElseIf length = 1 AndAlso bytes(valueStartIndex) = &Hfe Then
						currentRow.Add(Nothing)
					Else
						Dim valueBytes As Byte() = bytes.Skip(valueStartIndex).Take(length).ToArray()
						currentRow.Add(decoder.GetString(valueBytes))
					End If
					valueStartIndex = i + 1
				ElseIf bytes(i) = &Hfd Then
					If i > 0 AndAlso valueStartIndex <> i Then
						Throw New Exception("Incomplete RSV row")
					End If
					result.Add(currentRow.ToArray())
					currentRow.Clear()
					valueStartIndex = i + 1
				End If
			Next
			Return result.ToArray()
		End Function

		' ----------------------------------------------------------------------

		Private Shared Sub SaveRsv(rows As String()(), filePath As String)
			File.WriteAllBytes(filePath, EncodeRsv(rows))
		End Sub

		Private Shared Function LoadRsv(filePath As String) As String()()
			Return DecodeRsv(File.ReadAllBytes(filePath))
		End Function

		Private Shared Sub AppendRsv(rows As String()(), filePath As String, Optional continueLastRow As Boolean = False)
			Using fileStream As FileStream = File.Open(filePath, FileMode.OpenOrCreate, FileAccess.ReadWrite)
				If continueLastRow AndAlso fileStream.Length > 0 Then
					fileStream.Position = fileStream.Length - 1
					If fileStream.ReadByte() <> &Hfd Then
						Throw New Exception("Incomplete RSV document")
					End If
					If rows.Length = 0 Then
						Return
					End If
					fileStream.Position = fileStream.Length - 1
				Else
					fileStream.Position = fileStream.Length
				End If
				Dim bytes As Byte() = EncodeRsv(rows)
				fileStream.Write(bytes, 0, bytes.Length)
			End Using
		End Sub

		' ----------------------------------------------------------------------

		Private Shared Function IsValidRsv(bytes As Byte()) As Boolean
			Dim byteClassLookup As Byte() = {
				1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
				1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
				1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
				1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
				1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
				1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
				1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
				1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
				2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
				3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
				4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
				4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
				0, 0, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
				5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
				6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 7, 7,
				9, 10, 10, 10, 11, 0, 0, 0, 0, 0, 0, 0, 0, 12, 13, 14
			}
			Dim stateTransitionLookup As Byte() = {
				0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
				0, 2, 0, 0, 0, 3, 4, 6, 5, 7, 8, 9, 1, 10, 11,
				0, 2, 0, 0, 0, 3, 4, 6, 5, 7, 8, 9, 0, 0, 11,
				0, 0, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
				0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
				0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
				0, 0, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
				0, 0, 0, 6, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
				0, 0, 6, 6, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
				0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
				0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11,
				0, 2, 0, 0, 0, 3, 4, 6, 5, 7, 8, 9, 1, 10, 11
			}
			Dim lastState As Integer = 1
			For i As Integer = 0 To bytes.Length - 1
				Dim currentByte As Byte = bytes(i)
				Dim currentByteClass As Integer = byteClassLookup(currentByte)
				Dim newStateLookupIndex As Integer = lastState * 15 + currentByteClass
				lastState = stateTransitionLookup(newStateLookupIndex)
				If lastState = 0 Then
					Return False
				End If
			Next
			Return (lastState = 1)
		End Function

		' ----------------------------------------------------------------------

		Private Shared Function ByteArrayToString(bytes As Byte()) As String
			Return "[" & String.Join(", ", bytes) & "]"
		End Function

		' ----------------------------------------------------------------------

		Private Shared Function EscapeJsonString(str As String) As String
			Dim result As New StringBuilder()
			result.Append("""")
			For i As Integer = 0 To str.Length - 1
				Dim c As Integer = AscW(str(i))
				If c = &H8 Then
					result.Append("\b")
				ElseIf c = &H9 Then
					result.Append("\t")
				ElseIf c = &Ha Then
					result.Append("\n")
				ElseIf c = &Hc Then
					result.Append("\f")
				ElseIf c = &Hd Then
					result.Append("\r")
				ElseIf c = &H22 Then
					result.Append("\""")
				ElseIf c = &H5c Then
					result.Append("\\")
				ElseIf c >= &H0 AndAlso c <= &H1f Then
					result.AppendFormat("\u{0:x4}", c)
				Else
					result.Append(ChrW(c))
				End If
			Next
			result.Append("""")
			Return result.ToString()
		End Function

		Private Shared Function RsvToJson(rows As String()()) As String
			Return "[" & (If(rows.Length > 0, vbLf, "")) & [String].Join("," & vbLf, rows.[Select](Function(row) "  [" & [String].Join(", ", row.[Select](Function(x) If(x Is Nothing, "null", EscapeJsonString(x)))) & "]")) & vbLf & "]"
		End Function

		Private Shared Sub PrintRsvToJson(rows As String()())
			Console.WriteLine(RsvToJson(rows))
		End Sub

		' ----------------------------------------------------------------------

		Private Shared Sub CheckTestFiles()
			For i As Integer = 1 To 79
				Dim filePath As String = ".\..\TestFiles\Valid_" & i.ToString("D3")
				Console.WriteLine("Checking valid test file: " & filePath)
				Dim loadedRows As String()() = LoadRsv(filePath & ".rsv")
				Dim jsonStr As String = RsvToJson(loadedRows)
				
				Dim loadedJsonStr As String = File.ReadAllText(filePath & ".json")
				If jsonStr <> loadedJsonStr Then
					Throw New Exception("JSON mismatch")
				End If

				If Not IsValidRsv(File.ReadAllBytes(filePath & ".rsv")) Then
					Throw New Exception("Validation mismatch")
				End If
			Next

			For i As Integer = 1 To 29
				Dim filePath As String = ".\..\TestFiles\Invalid_" & i.ToString("D3")
				Console.WriteLine("Checking invalid test file: " & filePath)
				Dim wasError As Boolean = False
				Try
					Dim loadedRows As String()() = LoadRsv(filePath & ".rsv")
				Catch e As Exception
					wasError = True
				End Try
				If Not wasError Then
					Throw New Exception("RSV document is valid")
				End If

				If IsValidRsv(File.ReadAllBytes(filePath & ".rsv")) Then
					Throw New Exception("Validation mismatch")
				End If
			Next
		End Sub

		Public Shared Sub Main(args As String())
			Dim rows As String()() = {
				New String() {"Hello", "🌎", Nothing, ""},
				New String() {"A" & vbNullChar & "B" & vbLf & "C", "Test 𝄞"},
				New String() {},
				New String() {""}
			}

			Console.OutputEncoding = System.Text.Encoding.UTF8

			PrintRsvToJson(rows)
			Dim bytes As Byte() = EncodeRsv(rows)
			'Console.WriteLine(ByteArrayToString(bytes));
			Dim decodedRows As String()() = DecodeRsv(bytes)
			SaveRsv(rows, "Test.rsv")

			Dim loadedRows As String()() = LoadRsv("Test.rsv")
			PrintRsvToJson(loadedRows)
			SaveRsv(loadedRows, "TestResaved.rsv")

			AppendRsv({New String() {"ABC"}}, "Append.rsv", False)

			CheckTestFiles()

			Console.WriteLine("Done")
			Console.ReadKey(True)
		End Sub
	End Class
End Namespace