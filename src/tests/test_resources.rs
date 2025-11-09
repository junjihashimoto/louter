use base64::{Engine as _, engine::general_purpose::STANDARD as BASE64};
use std::fs;
use std::path::Path;

#[allow(dead_code)]
pub fn load_test_image_as_base64(filename: &str) -> String {
    let path = Path::new("test-resources").join(filename);
    let data = fs::read(&path)
        .unwrap_or_else(|_| panic!("Failed to read test image: {:?}", path));
    BASE64.encode(data)
}

#[allow(dead_code)]
pub fn load_test_pdf_as_base64() -> String {
    load_test_image_as_base64("test.pdf")
}

#[allow(dead_code)]
pub fn load_test_audio_as_base64() -> String {
    load_test_image_as_base64("silence.wav")
}

#[allow(dead_code)]
pub fn get_mime_type(filename: &str) -> &'static str {
    match filename.split('.').last() {
        Some("png") => "image/png",
        Some("jpg") | Some("jpeg") => "image/jpeg",
        Some("bmp") => "image/bmp",
        Some("pdf") => "application/pdf",
        Some("wav") => "audio/wav",
        Some("mp4") => "video/mp4",
        _ => "application/octet-stream",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_load_png_image() {
        let base64_data = load_test_image_as_base64("red_pixel.png");
        assert!(!base64_data.is_empty());
        // Verify it starts like a PNG
        let decoded = BASE64.decode(&base64_data).unwrap();
        assert_eq!(&decoded[1..4], b"PNG");
    }

    #[test]
    fn test_load_jpeg_image() {
        let base64_data = load_test_image_as_base64("blue_square.jpg");
        assert!(!base64_data.is_empty());
        // Verify it starts like a JPEG
        let decoded = BASE64.decode(&base64_data).unwrap();
        assert_eq!(&decoded[0..2], &[0xFF, 0xD8]);
    }

    #[test]
    fn test_load_pdf() {
        let base64_data = load_test_pdf_as_base64();
        assert!(!base64_data.is_empty());
        // Verify it starts like a PDF
        let decoded = BASE64.decode(&base64_data).unwrap();
        assert_eq!(&decoded[0..4], b"%PDF");
    }

    #[test]
    fn test_load_audio() {
        let base64_data = load_test_audio_as_base64();
        assert!(!base64_data.is_empty());
        // Verify it starts like a WAV file
        let decoded = BASE64.decode(&base64_data).unwrap();
        assert_eq!(&decoded[0..4], b"RIFF");
        assert_eq!(&decoded[8..12], b"WAVE");
    }

    #[test]
    fn test_mime_types() {
        assert_eq!(get_mime_type("test.png"), "image/png");
        assert_eq!(get_mime_type("test.jpg"), "image/jpeg");
        assert_eq!(get_mime_type("test.jpeg"), "image/jpeg");
        assert_eq!(get_mime_type("test.pdf"), "application/pdf");
        assert_eq!(get_mime_type("test.wav"), "audio/wav");
        assert_eq!(get_mime_type("test.mp4"), "video/mp4");
        assert_eq!(get_mime_type("test.unknown"), "application/octet-stream");
    }
}